(package klvm-trans [klvm-reg->
                     klvm-nregs->
                     klvm-call
                     ]

\\
\\ Optimization for passing arguments to tailcalled function
\\

(define place-constants
  [] _ Args Acc -> (@p Acc (reverse Args))
  [[C] | Xs] I Args Acc -> (let Acc [[I | [C]] | Acc]
                                Args [[] | Args]
                             (place-constants Xs (+ I 1) Args Acc))
  [X | Xs] I Args Acc -> (place-constants Xs (+ I 1) [X | Args] Acc))

(define place-free-args
  [] _ _ Args Acc -> (@p Acc (reverse Args))
  [[] | Xs] I Map Args Acc -> (place-free-args Xs (+ I 1) Map [[] | Args] Acc)
  [X | Xs] I Map Args Acc -> (place-free-args Xs (+ I 1) Map [X | Args] Acc)
                             where (element? I Map)
  [X | Xs] I Map Args Acc -> (let Acc [[I | X] | Acc]
                                  Args [[] | Args]
                               (place-free-args Xs (+ I 1) Map Args Acc)))

(define mk-place
  [] I -> I
  [[_ | I] | Xs] _ -> (mk-place Xs (+ I 1)))

(define place-args'
  [] _ Map Acc -> (@p Acc Map)
  [[] | Xs] I Map Acc -> (place-args' Xs (+ I 1) Map Acc)
  [X | Xs] I Map Acc -> (place-args' Xs (+ I 1) Map [[I | X] | Acc])
                        where (not (element? I Xs))
  [X | Xs] I Map Acc -> (let Place (mk-place Map 0)
                             Acc [[Place | X] | Acc]
                             Map [[I | Place] | Map]
                          (place-args' Xs (+ I 1) Map Acc)))

(define place-extra
  [] Acc -> (reverse Acc)
  [[[] | _]] Acc -> (reverse Acc)
  [[D | S] | Ps] Acc -> (place-extra Ps [[D | S] | Acc]))

(define nplaces
  [] N -> N
  [[I | _] | Ps] N -> (nplaces Ps N) where (> N I)
  [[I | _] | Ps] N -> (nplaces Ps I))

(define place-args
  Nregs Args -> (let X (place-constants Args 1 [] [])
                     X (place-free-args (snd X) 1 (snd X) [] (fst X))
                     X (place-args' (snd X) 1 [[[] | Nregs]] (fst X))
                     X (place-extra (snd X) (fst X))
                  (@p X (nplaces X 0))))

(define set-tailcall-args'
  [] Acc -> Acc
  [[D | D] | Ps] Acc -> (set-tailcall-args' Ps Acc)
  [[D | [C]] | Ps] Acc -> (let Acc [[klvm-reg-> [D] C] | Acc]
                            (set-tailcall-args' Ps Acc))
  [[D | S] | Ps] Acc -> (let Acc [[klvm-reg-> [D] [klvm-reg S]] | Acc]
                          (set-tailcall-args' Ps Acc)))

(define set-tailcall-args
  Nregs Args Acc -> (let P (place-args Nregs Args)
                         Acc [[klvm-nregs-> [(snd P)]] | Acc]
                      (@p (set-tailcall-args' (fst P) Acc) (snd P))))

(define reg-from-arg
  [klvm-reg R | _] -> R
  X -> [X] where (not (cons? X))
  X -> (error "Unexpected arg expression ~R" X))

(define regs-from-args
  [] Acc -> Acc
  [X | Xs] Acc -> (regs-from-args Xs [(reg-from-arg X) | Acc]))

(define emit-tailcall-args
  Args C Nargs+ Acc ->
  (let R (regs-from-args Args [])
       X (set-tailcall-args (context-nregs C) R Acc)
       Nargs (length Args)
       Nargs-reg (+ (context-nargs C) (context-nregs C) 2)
       Acc [[klvm-nregs-> [Nargs | Nargs+]] | (fst X)]
       Acc [[klvm-nargs-> Nargs] | Acc]
       Acc [[klvm-inc-nargs [klvm-reg Nargs-reg]] | Acc]
    (@p Acc (snd X))))

(define emit-tailcall
  F Args C Acc -> (let X (emit-tailcall-args Args C [] Acc)
                       S (context-stack-size C)
                       Acc [[klvm-dec-stack-ptr [klvm-reg S]] | (fst X)]
                    [[klvm-call F] | Acc])
                  where (or (and (symbol? F) (= (context-bind-funcs C) all))
                            (= F (context-func-name C)))
  F Args C Acc -> (let Acc [[klvm-closure-> (imp-expr3 F C)] | Acc]
                       N [klvm-closure-nargs]
                       X (emit-tailcall-args Args [N] C Acc)
                       Acc [[klvm-put-closure-args (snd Acc)] | (fst Acc)]
                       X (context-stack-size C)
                       Acc [[klvm-dec-stack-ptr [klvm-reg X]] | Acc]
                    [[klvm-call [klvm-closure-func]] | Acc]))

(define set-call-args 
  [] _ Acc -> Acc
  [A | As] I Acc -> (set-call-args As (+ I 1) [[klvm-reg-> [I] A] | Acc]))

(define emit-call-args
  Args Nargs+ C Acc ->
  (let R (regs-from-args Args [])
       Nargs (length R)
       Acc [[klvm-nregs-> [(+ (context-stack-size C) Nargs) | Nargs+]] | Acc]
       Acc [[klvm-nargs-> Nargs] | Acc]
       X (set-call-args R 1 Acc)
    Acc))

(define emit-call
  F Args Return-reg C Acc -> (let Acc (emit-call-args Args [] C Acc)
                                  Nargs (length Args)
                                  Stacksize (context-stack-size C)
                                  Acc [[klvm-inc-stack-ptr Stacksize] | Acc]
                                  Acc [[klvm-call F] | Acc]
                                  Acc (label (next-label C) C Acc)
                               (emit-receive Return-reg C Acc))
                             where (and (symbol? F)
                                        (= (context-bind-funcs C) all))
  F Args Return-reg C Acc -> (let N [klvm-closure-nargs]
                                  Nargs (length Args)
                                  Stacksize (context-stack-size C)
                                  Acc (emit-call-args Args [[N]] C Acc)
                                  Acc [[klvm-closure-> F] | Acc]
                                  Acc [[klvm-put-closure-args Stacksize]
                                       | Acc]
                                  Acc [[klvm-inc-stack-ptr Stacksize] | Acc]
                                  Acc (label (next-label C) C Acc)
                               (emit-receive Return-reg C Acc)))

(define emit-receive
  [] C Acc -> [[klvm-dec-stack-ptr (context-stack-size C)] | Acc]
  Return-reg C Acc -> (let Acc [[klvm-reg-> [Return-reg] ]]
                           Acc [[klvm-dec-stack-ptr (context-stack-size C)]
                                | Acc]

(define test-emit-tailcall
  F Args Nregs -> (let C (mk-context 3 5 0 [] [] [] _ _)
                    (emit-tailcall F Args C [])))
)
