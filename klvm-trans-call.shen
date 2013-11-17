(package klvm-trans [klvm-reg->
                     klvm-reg
                     klvm-nregs->
                     klvm-nargs
                     klvm-nargs->
                     klvm-inc-nargs
                     klvm-inc-stack-ptr
                     klvm-dec-stack-ptr
                     klvm-save-stack-ptr
                     klvm-restore-stack-ptr
                     klvm-wipe-stack
                     klvm-call
                     klvm-closure->
                     klvm-closure-func
                     klvm-closure-nargs
                     klvm-put-closure-args
                     klvm-next
                     klvm-next->
                     ]

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

(define max
  X Y -> X where (> X Y)
  _ Y -> Y)

(define place-args
  Nregs Args -> (let X (place-free-args Args 0 Args [] [])
                     X (place-args' (snd X) 0 [[[] | Nregs]] (fst X))
                     X (place-extra (snd X) (fst X))
                  (@p X (nplaces X 0))))

(define max-reg-id
  [] M -> M
  [[D | X] | Ps] M -> (max-reg-id Ps D) where (> D M)
  [[D | X] | Ps] M -> (max-reg-id Ps M))

(define set-tailcall-args'
  [] Acc -> Acc
  [[D | D] | Ps] Acc -> (set-tailcall-args' Ps Acc)
  [[D | [C]] | Ps] Acc -> (let Acc [[klvm-reg-> D C] | Acc]
                            (set-tailcall-args' Ps Acc))
  [[D | S] | Ps] Acc -> (let Acc [[klvm-reg-> D [klvm-reg S]] | Acc]
                          (set-tailcall-args' Ps Acc)))

(define set-tailcall-args
  Nregs Args Acc -> (let P (place-args Nregs Args)
                         Acc [[klvm-nregs-> [(+ (snd P) 1)]] | Acc]
                         Acc (set-tailcall-args' (fst P) Acc)
                      (@p Acc (snd P))))

(define reg-from-arg
  [klvm-reg R | _] -> R
  X -> [X])

(define regs-from-args
  [] Acc -> Acc
  [X | Xs] Acc -> (regs-from-args Xs [(reg-from-arg X) | Acc]))

(define emit-tailcall-args
  Args C Nargs+ Acc ->
  (let R (regs-from-args Args [])
       Nargs (length Args)
       Nargs-reg (func-nargs-reg C)
       Acc [[klvm-nargs-> [klvm-reg (func-nargs-reg C)]] | Acc]
       Acc [[klvm-dec-stack-ptr [klvm-nargs]] | Acc]
       Acc [[klvm-save-stack-ptr] | Acc]
       Acc [[klvm-inc-stack-ptr [klvm-nargs]] | Acc]
       Acc [[klvm-next-> [klvm-reg (func-next-reg C)]] | Acc]
       Acc [[klvm-nargs-> Nargs] | Acc]
       Acc [[klvm-inc-nargs [klvm-reg Nargs-reg]] | Acc]
       X (set-tailcall-args (context-stack-size C) R Acc)
       Acc [[klvm-nregs-> [Nargs | Nargs+]] | (fst X)]
    (@p Acc (snd X))))

(define emit-tailcall
  F Args C Acc -> (let R (func-nargs-reg C)
                       X (emit-tailcall-args Args C [] Acc)
                       Acc [[klvm-restore-stack-ptr] | (fst X)]
                    [[klvm-call F] | Acc])
                  where (or (and (symbol? F) (= (context-bind-funcs C) all))
                            (and (= F (context-func-name C))
                                 (= (context-bind-funcs C) self)))
  F Args C Acc -> (let Acc [[klvm-closure-> (emit-expr3 F C)] | Acc]
                       N [klvm-closure-nargs]
                       X (emit-tailcall-args Args C [N] Acc)
                       Acc [[klvm-put-closure-args (snd X)] | (fst X)]
                       Acc [[klvm-inc-nargs N] | Acc]
                       Acc [[klvm-restore-stack-ptr] | Acc]
                    [[klvm-call [klvm-closure-func]] | Acc]))

(define set-call-args 
  [] _ Acc -> Acc
  [A | As] I Acc -> (let X [klvm-reg A]
                      (set-call-args As (+ I 1) [[klvm-reg-> I X] | Acc]))
                    where (number? A)
  [[A] | As] I Acc -> (set-call-args As (+ I 1) [[klvm-reg-> I A] | Acc])
  [X | As] _ _ -> (error "Unexpected arg expr: ~S~%" X))

(define emit-call-args
  Args Nargs+ C Acc ->
  (let R (regs-from-args Args [])
       Nargs (length R)
       Acc [[klvm-next-> (+ (context-label C) 1)] | Acc]
       Acc [[klvm-nregs-> [(+ (context-stack-size C) Nargs) | Nargs+]] | Acc]
       Acc [[klvm-nargs-> Nargs] | Acc]
       Acc (set-call-args R (context-stack-size C) Acc)
    Acc))

(define emit-call
  F Args Return-reg C Acc -> (let Nargs (length Args)
                                  Stacksize (context-stack-size C)
                                  Acc (emit-call-args Args [] C Acc)
                                  Acc [[klvm-inc-stack-ptr Stacksize] | Acc]
                                  Acc [[klvm-call F] | Acc]
                                  Acc (label (next-label C) C Acc)
                               (emit-receive Return-reg C Acc))
                             where (and (symbol? F) (= (context-bind-funcs C)
                                                       all))
  F Args Return-reg C Acc -> (let N [klvm-closure-nargs]
                                  Nargs (length Args)
                                  Stacksize (context-stack-size C)
                                  Acc [[klvm-closure-> (emit-expr3 F C)]
                                       | Acc]
                                  Acc (emit-call-args Args [N] C Acc)
                                  Acc [[klvm-put-closure-args Stacksize]
                                       | Acc]
                                  Acc [[klvm-inc-stack-ptr Stacksize] | Acc]
                                  Acc [[klvm-inc-nargs N] | Acc]
                                  Acc [[klvm-call [klvm-closure-func]] | Acc]
                                  Acc (label (next-label C) C Acc)
                               (emit-receive Return-reg C Acc)))

(define emit-receive
  [] C Acc -> (let S (context-stack-size C)
                   Acc [[klvm-dec-stack-ptr S] | Acc]
                [[klvm-wipe-stack S] | Acc])
  Return-reg C Acc -> (let S (context-stack-size C)
                           Acc [[klvm-dec-stack-ptr S] | Acc]
                           Acc [[klvm-reg-> Return-reg [klvm-reg S]] | Acc]
                           Acc [[klvm-wipe-stack S] | Acc]
                        Acc))

(define test-emit-tailcall
  F Args -> (let C (mk-context two 3 1 0 0 [] [] [] _ _)
              (emit-tailcall F Args C [])))

)

(define klvm-trans.test-emit-tailcall-1
  -> (let X (klvm-trans.test-emit-tailcall one [[klvm-reg 0] x])
          - (nl)
       (klvm-trans.show-code (reverse X))))

(define klvm-trans.test-closure
  -> (let C (klvm-trans.mk-context two 3 1 0 0 [] [] [] _ klvm-trans.null-fn)
          X (klvm-trans.closure [] [] 3 [[shen-get-reg 2]] [] C [])
          - (nl)
       (klvm-trans.show-code (reverse X))))

(define klvm-trans.test-call1
  -> (let X (read-from-string "(define mapx Fn List -> (mapx' Fn List []))")
          Kl (kl-from-shen X)
          Klvm (klvm-from-kl klvm-trans.null-fn Kl)
       (klvm-trans.show-code (reverse Klvm))))
