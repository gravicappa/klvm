(package klvm.s1 [denest.walk klvm-dump
                  regkl.walk regkl.get-arg regkl.get-reg regkl.set-reg!
                  regkl.closure regkl.func regkl.toplevel regkl.freeze]
[])

(defstruct context
  (func symbol)
  (nregs number)
  (nargs number)
  (stack-size number)
  (max-extra-stack-size number)
  (toplevel s-expr)
  (native (A --> context --> A)))

(define warning X -> (output "Warning: ~A" X))
(define warn-type -> (warning "`type` expression is not supported yet"))

(define const?
  X -> true where (symbol? X)
  X -> true where (string? X)
  X -> true where (number? X)
  X -> true where (boolean? X)
  _ -> false)

(define func-reg X C -> (+ (context-nargs C) X))
(define func-arg X C -> (- (context-nargs C) (+ X 1)))
(define ret-reg C -> (context-stack-size C))

(define upd-context-max-extra-stack-size
  N C -> (context-max-extra-stack-size-> C N)
         where (> N (context-max-extra-stack-size C))
  _ C -> C)

(define reg-from-arg
  [regkl.get-reg R | _] C -> (func-reg R C)
  [regkl.get-arg R | _] C -> (func-arg R C)
  X _ -> [X])

(define regs-from-args
  [] C Acc -> Acc
  [X | Xs] C Acc -> (regs-from-args Xs C [(reg-from-arg X C) | Acc]))

(define set-call-args 
  [] _ Acc -> Acc
  [A | As] I Acc -> (let X [klvm-reg A]
                      (set-call-args As (+ I 1) [[klvm.reg-> I X] | Acc]))
                    where (number? A)
  [[A] | As] I Acc -> (set-call-args As (+ I 1) [[klvm.reg-> I A] | Acc])
  [X | As] _ _ -> (error "Unexpected arg expr: ~S~%" X))

(define walk-call
  F Args Return-reg C Acc ->
  (let R (regs-from-args Args C [])
       Nargs (length R)
       . (upd-context-max-extra-stack-size Nargs C)
       Acc (set-call-args R (context-stack-size C) Acc)
    [[klvm.call F Nargs Return-reg] | Acc]))

(define place-free-args
  [] _ _ Args Acc -> (@p Acc (reverse Args))
  [[] | Xs] I Map Args Acc -> (place-free-args Xs (+ I 1) Map [[] | Args] Acc)
  [X | Xs] I Map Args Acc -> (place-free-args Xs (+ I 1) Map [X | Args] Acc)
                             where (element? I Map)
  [X | Xs] I Map Args Acc -> (let Acc [[I | X] | Acc]
                                  Args [[] | Args]
                               (place-free-args Xs (+ I 1) Map Args Acc)))

(define mk-place
  [] M -> (+ M 1)
  [[_ | I] | Xs] M -> (mk-place Xs M) where (> M I)
  [[_ | I] | Xs] M -> (mk-place Xs I))

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
  Nregs Args -> (let X (place-free-args Args 0 Args [] [])
                     X (place-args' (snd X) 0 [[[] | Nregs]] (fst X))
                     X (place-extra (snd X) (fst X))
                  (@p X (nplaces X 0))))

(define set-tailcall-args'
  [] Acc -> Acc
  [[D | D] | Ps] Acc -> (set-tailcall-args' Ps Acc)
  [[D | [C]] | Ps] Acc -> (let Acc [[reg-> D C] | Acc]
                            (set-tailcall-args' Ps Acc))
  [[D | S] | Ps] Acc -> (let Acc [[reg-> D [reg S]] | Acc]
                          (set-tailcall-args' Ps Acc)))

(define set-tailcall-args
  Nregs Args Acc -> (let P (place-args Nregs Args)
                         Acc [[nregs-> [(+ (snd P) 1)]] | Acc]
                         Acc (set-tailcall-args' (fst P) Acc)
                      (@p Acc (snd P))))

(define walk-tailcall
  F Args C Acc ->
  (let R (regs-from-args Args C [])
       Nargs (length R)
       . (upd-context-max-extra-stack-size Nargs C)
       X (set-tailcall-args (context-stack-size C) R Acc)
    [[klvm.tailcall F] | (fst X)]))

(define walk-closure
  Return-reg Args Nregs Init Body C Acc ->
  (let Ninit (length Init)
       Nargs (+ Ninit (length Args))
       F (gensym klvm-lambda)
       TL (context-toplevel C)
       A (klvm-trans.closure-args (protect A) 0 Nargs [])
       Fn (context-native C)
       . (context-toplevel-> C (walk-func regkl.closure F A Nregs Body Fn TL))
    (if (= Return-reg [])
        (walk-tailcall F Init C Acc)
        (walk-call F Init Return-reg C Acc))))

(define walk-freeze
  Tgt-reg Nregs Init Body C Acc ->
  (walk-closure Tgt-reg [] Nregs Init Body C Acc))

(define walk-x3
  [type X Type] C -> (do (warn-type)
                         (walk-x3 X C))
  [regkl.get-reg R] C -> [reg (func-reg R C)]
  [regkl.get-arg R] C -> [reg (func-arg R C)]
  X C -> X where (const? X)
  X _ -> (error "Unexpected L3 Reg-KLambda ~S." X))

(define walk-x2
  [type X Type] Return-reg C Acc -> (do (warn-type)
                                        (walk-x2 X Return-reg C Acc))
  
  [regkl.closure Args Nregs Init Body] Return-reg C Acc ->
    (walk-closure Return-reg Args Nregs Init Body C Acc)
  
  [regkl.freeze Nregs Init Body] Return-reg C Acc ->
    (walk-freeze Return-reg Nregs Init Body C Acc)
  
  [regkl.get-reg R] Return-reg C Acc ->
    [[reg-> Return-reg [reg (func-reg R C)]] | Acc]
  
  [regkl.get-arg R] Return-reg C Acc ->
    [[reg-> Return-reg [reg (func-arg R C)]] | Acc]
  
  [F | Args] Return-reg C Acc -> (walk-call F Args Return-reg C Acc)
  X Return-reg C Acc -> [[reg-> Return-reg X] | Acc] where (const? X)
  X _ _ _ -> (error "Unexpected L2 Reg-KLambda expression ~S" X))

(define walk-if-reg
  R Then Else Tail? C Acc -> (let T' (head (walk-x1 Then false Tail? C []))
                                  E' (head (walk-x1 Else false Tail? C []))
                               [[if [reg R] T' E'] | Acc]))

(define walk-if
  [regkl.get-reg R] Then Else Tail? C Acc ->
  (walk-if-reg (func-reg R C) Then Else Tail? C Acc)

  [regkl.get-arg R] Then Else Tail? C Acc ->
  (walk-if-reg (func-arg R C) Then Else Tail? C Acc)

  X _ _ _ _ _ -> (error "Broken Reg-KLambda ~S." X))

(define walk-do
  [X] Tail? C Acc -> (walk-x1 X true Tail? C Acc)
  [X | Rest] Tail? C Acc -> (let Acc' (walk-x1 X true false C Acc)
                              (walk-do Rest Tail? C Acc')))

(define in-do
  Fn true Acc -> (Fn Acc)
  Fn false Acc -> [[do | (reverse (Fn []))] | Acc])

(define walk-x1
  \\X _ _ _ _ <- (do (output "walk-x1: ~S~%" X) (fail))
  [do | X] Do? Tail? C Acc -> (in-do (walk-do X Tail? C) Do? Acc)
  [if If Then Else] _ Tail? C Acc -> (walk-if If Then Else Tail? C Acc)
  [regkl.get-reg R] _ true C Acc -> [[reg (func-reg R C)] | Acc]
  [regkl.get-reg _] _ false _ Acc -> Acc
  [regkl.get-arg R] _ true C Acc -> [[reg (func-arg R C)] | Acc]
  [regkl.get-arg _] _ false _ Acc -> Acc
  [regkl.closure | _] _ false _ Acc ->  Acc
  [regkl.freeze | _ ] _ false _ Acc -> Acc

  [regkl.set-reg! R X2] Do? false C Acc ->
  (in-do (walk-x2 X2 (func-reg R C) C) Do? Acc)

  [regkl.closure Args Nregs Init Body] Do? true C Acc ->
  (in-do (walk-closure [] Args Nregs Init Body C) Do? Acc)

  [regkl.freeze Nregs Init Body] Do? true C Acc ->
  (in-do (walk-freeze [] Nregs Init Body C) Do? Acc)

  [F | Args] Do? true C Acc -> (in-do (walk-tailcall F Args C) Do? Acc)
  [F | Args] Do? false C Acc -> (in-do (walk-call F Args [] C) Do? Acc)
  _ _ false _ Acc -> Acc
  X _ true _ Acc -> [X | Acc]
  X _ _ _ _ -> (error "Unexpected L1 Reg-KLambda expression ~S" X))

(define func-code
  regkl.func -> func
  regkl.toplevel -> toplevel
  regkl.closure -> closure)

(define walk-func
  Type Name Args Nregs Body Fn Toplevel -> 
  (let Nargs (length Args)
       C (mk-context Name Nregs Nargs (+ Nregs Nargs 2) 0 Toplevel Fn)
       Body' (walk-x1 Body false true C [])
       X [(func-code Type) Name Args Nregs | (reverse Body')]
    [X | (context-toplevel C)]))

(define walk-1
  [Type Name Args Nregs Body] Fn Toplevel -> 
  (walk-func Type Name Args Nregs Body Fn Toplevel)
  where (element? Type [regkl.func regkl.toplevel regkl.closure])
  [X | Y] _ _ -> (error "Unexpected toplevel expression ~S~%" [X | Y])
  X _ Toplevel -> Toplevel)

(define walk-toplevel'
  [] _ Acc -> (reverse Acc)
  [X | Y] Fn Acc -> (walk-toplevel' Y Fn (walk-1 X Fn Acc)))

(define klvm.s1.walk
  Fn X -> (let X' (regkl.walk (map (function denest.walk) X) false)
               . (output "~%*REGKL:~%~S~%~%" X')
            (walk-toplevel' X' Fn [])))

(define test-tailcall
  -> (set-tailcall-args 5 [3 2 [4]] []))
