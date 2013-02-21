(defstruct impcontext
  (nargs number)
  (nregs number)
  (label number)
  (func s-expr)
  (toplevel s-expr)
  (primitives (list symbol))
  (bind-funcs symbol))

(define kl-imp-next-label
  C -> (do (impcontext-label-> C (+ (impcontext-label C) 1))
           (impcontext-label C)))

(define kl-imp-close-label
  C [] -> []
  C Acc -> (do (impcontext-func-> C [(reverse Acc) | (impcontext-func C)])
               []))

(define kl-imp-label
  N C Acc -> (do (kl-imp-close-label C Acc)
                 [[klvm-label N]]))

(define kl-imp-expr3
  [type | X] _ Acc -> [[type | X] | Acc]
  [shen-get-reg R] C Acc -> [[klvm-reg (+ (impcontext-nargs C) R 1)] | Acc]
  [shen-get-arg R] C Acc -> [[klvm-reg (+ R 1)] | Acc]
  _ _ _ -> (fail))

(define kl-imp-push-stack-aux
  N N _ Acc -> Acc
  Except N Except Acc -> (kl-imp-push-stack-aux (+ Except 1) N Except Acc)
  I N Except Acc -> (let Acc [[klvm-stack-> (+ I 1) [klvm-reg I]] | Acc]
                      (kl-imp-push-stack-aux (+ I 1) N Except Acc)))

(define kl-imp-push-stack-n
  N Except Acc -> (kl-imp-push-stack-aux 0 N Except Acc))

(define kl-imp-push-stack
  Except C Acc -> (let N (+ (impcontext-nargs C) (impcontext-nregs C) 1)
                    (kl-imp-push-stack-n N Except Acc)))

(define kl-imp-pop-stack-aux
  N N _ Acc -> Acc
  Except N Except Acc -> (kl-imp-pop-stack-aux (+ Except 1) N Except Acc)
  I N Except Acc -> (let Acc [[klvm-reg-> [I] [klvm-stack (+ I 1)]] | Acc]
                      (kl-imp-pop-stack-aux (+ I 1) N Except Acc)))

(define kl-imp-pop-stack-n
  N Except Acc -> (let Acc [[klvm-dec-stack-ptr (+ N 1)] | Acc]
                    (kl-imp-pop-stack-aux 0 N Except Acc)))

(define kl-imp-pop-stack
  Except C Acc -> (let N (+ (impcontext-nargs C) (impcontext-nregs C) 1)
                    (kl-imp-pop-stack-n N Except Acc)))

(define kl-imp-stack-ptr
  Op C Acc -> (let N (+ (impcontext-nargs C) (impcontext-nregs C) 2)
                (if (= N 0)
                    Acc
                    [[Op N] | Acc])))

(define kl-imp-inc-stack-ptr
  C Acc -> (kl-imp-stack-ptr klvm-inc-stack-ptr C Acc))

(define kl-imp-dec-stack-ptr
  C Acc -> (kl-imp-stack-ptr klvm-dec-stack-ptr C Acc))

(define kl-imp-set-args
  Off _ [] _ Acc -> Acc

  Off I [[shen-get-arg X] | Y] C Acc ->
  (let Acc [[klvm-reg-> [(+ I 1) | Off] [klvm-stack (+ X 2)]] | Acc]
    (kl-imp-set-args Off (+ I 1) Y C Acc))

  Off I [[shen-get-reg X] | Y] C Acc ->
  (let X' (+ (impcontext-nargs C) X 2)
       Acc [[klvm-reg-> [(+ I 1) | Off] [klvm-stack X']] | Acc]
    (kl-imp-set-args Off (+ I 1) Y C Acc))

  Off I [X | Y] C Acc -> (let Acc [[klvm-reg-> [(+ I 1) | Off] X] | Acc]
                           (kl-imp-set-args Off (+ I 1) Y C Acc)))

(define kl-imp-call-func
  F Nargs true C Acc -> [[klvm-call F] [klvm-nargs-> Nargs] | Acc]
                        where (and (symbol? F)
                                   (or (= (impcontext-bind-funcs C) all)
                                       (and (= (impcontext-bind-funcs C) sys)
                                            (shen-sysfunc? F))))

  F Nargs false C Acc ->
  [[klvm-call F] [klvm-nargs-> Nargs] | (kl-imp-inc-stack-ptr C Acc)]
  where (symbol? F)

  F Nargs true C Acc -> []

  F Nargs false C Acc ->
  (let N-regs (+ (impcontext-nargs C) (impcontext-nregs C))
       Acc (kl-imp-inc-stack-ptr C Acc)
       Acc [[klvm-nargs-> Nargs] | Acc]
       Acc [[klvm-inc-nargs [klvm-closure-nargs]] | Acc]
    [[klvm-call F] | Acc]))

(define kl-imp-use-call-ret
  [] C Acc -> (kl-imp-pop-stack _ C Acc)
  Return-reg C Acc -> (let X [klvm-reg-> [Return-reg] [klvm-reg 1]]
                        (kl-imp-pop-stack Return-reg C [X | Acc])))

(define kl-imp-call
  F Args Return-reg C Acc ->
  (let Acc (kl-imp-push-stack Return-reg C Acc)
       Acc [[klvm-nregs-> [(+ (length Args) 1)]] | Acc]
       Acc [[klvm-reg-> [0] (+ (impcontext-label C) 1)] | Acc]
       Acc (kl-imp-set-args [] 0 Args C Acc)
       X (if (cons? F)
             (head (kl-imp-expr3 F C []))
             F)
       Acc (kl-imp-call-func X (length Args) false C Acc)
       Acc (kl-imp-label (kl-imp-next-label C) C Acc)
    (kl-imp-use-call-ret Return-reg C Acc))
  where (symbol? F)

  F Args Return-reg C Acc ->
  (let Acc (kl-imp-push-stack Return-reg C Acc)
       X (kl-imp-expr3 F C [])
       Acc [[klvm-closure-> | X] | Acc]
       Acc [[klvm-nregs-> [(+ (length Args) 1) [klvm-closure-nargs]]] | Acc]
       Acc [[klvm-pop-closure-args | X] | Acc]
       Acc [[klvm-reg-> [0] (+ (impcontext-label C) 1)] | Acc]
       Acc (kl-imp-set-args [[klvm-closure-nargs]] 0 Args C Acc)
       Acc (kl-imp-call-func [klvm-closure-func] (length Args) false C Acc)
       Acc (kl-imp-label (kl-imp-next-label C) C Acc)
    (kl-imp-use-call-ret Return-reg C Acc)))

(define kl-imp-tailcall
  F Args C Acc -> (let N (length Args)
                       Acc (kl-imp-push-stack _ C Acc)
                       Acc (kl-imp-set-args [] 0 Args C Acc)
                       Acc [[klvm-nargs-> [klvm-stack 0]] | Acc]
                       Acc [[klvm-nregs-> [[klvm-nargs] N]] | Acc]
                       Acc [[klvm-dec-stack-ptr [klvm-nargs]] | Acc]
                       Acc [[klvm-pop-extra-args [klvm-nargs]] | Acc]
                       Acc [[klvm-inc-nargs N] | Acc]
                    [[klvm-call F] | Acc])
                   where (symbol? F)
  F Args C Acc -> (let N (length Args)
                       X (kl-imp-expr3 F C [])
                       Acc (kl-imp-push-stack _ C Acc)
                       Acc [[klvm-nargs-> [klvm-stack 0]] | Acc]
                       Acc [[klvm-closure-> | X] | Acc]
                       Acc [[klvm-nregs->
                             [[klvm-nargs] N [klvm-closure-nargs]]]
                            | Acc]
                       Acc [[klvm-pop-closure-args | X] | Acc]
                       Acc (kl-imp-set-args
                            [[klvm-closure-nargs]] 0 Args C Acc)
                       Acc [[klvm-dec-stack-ptr [klvm-nargs]] | Acc]
                       Acc [[klvm-pop-extra-args [klvm-nargs]] | Acc]
                       Acc [[klvm-inc-nargs N] | Acc]
                       Acc [[klvm-inc-nargs [klvm-closure-nargs]] | Acc]
                    [[klvm-call [klvm-closure-func]] | Acc]))

(define kl-imp-return-val
  Val Acc -> (let Acc [[klvm-nargs-> [klvm-stack 0]] | Acc]
                  X [klvm-nargs>0
                     [[klvm-nregs-> [[klvm-nargs]]]
                      [klvm-dec-stack-ptr [klvm-nargs]]
                      [klvm-pop-extra-args [klvm-nargs]]
                      [klvm-call Val]]
                     [[klvm-reg-> [1] Val]
                      [klvm-return]]]
               [X | Acc]))

(define kl-imp-return
  Target-reg Acc -> (kl-imp-return-val [klvm-reg Target-reg] Acc))


(define kl-imp-closure-init-aux
  I [] C Acc -> Acc
  I [X | Init] C Acc -> (let Y [klvm-stack-> I | (kl-imp-expr3 X C [])]
                          (kl-imp-closure-init-aux (+ I 1) Init C [Y | Acc])))

(define kl-imp-closure-init
  [] _ Acc -> Acc
  Init C Acc -> (let Acc (kl-imp-inc-stack-ptr C Acc)
                     Acc [[klvm-stack-size (length Init)] | Acc]
                     Acc (kl-imp-closure-init-aux 0 Init C Acc)
                  Acc))

(define kl-imp-closure-args
  S N N Acc -> (reverse Acc)
  S I N Acc -> (kl-imp-closure-args S (+ I 1) N [(concat S I) | Acc]))

(define kl-imp-mk-closure
  Tgt-reg Args Nregs Init Code C Acc ->
  (let Ninit (length Init)
       Nargs (+ Ninit (length Args))
       F (gensym klvm-lambda)
       TL (impcontext-toplevel C)
       A (kl-imp-closure-args (protect A) 0 Nargs [])
       TL (kl-imp-toplevel-expr [shen-mk-func F A Nregs Code] TL)
       _ (impcontext-toplevel-> C TL)
       Acc (kl-imp-closure-init Init C Acc)
       X [klvm-reg-> [Tgt-reg] [klvm-mk-closure F Nargs Ninit]]
       Acc [X | Acc]
    (if (= Ninit 0)
        Acc
        (kl-imp-dec-stack-ptr C Acc))))

(define kl-imp-closure
  [] Args Nregs Init Code C Acc ->
  (let Acc (kl-imp-mk-closure 1 Args Nregs Init Code C Acc)
    (kl-imp-return 1 Acc))
  Tgt-reg Args Nregs Init Code C Acc ->
  (let Acc (kl-imp-mk-closure Tgt-reg Args Nregs Init Code C Acc)
    [[klvm-stack-> (+ Tgt-reg 1) [klvm-reg Tgt-reg]] | Acc]))

(define kl-imp-mk-freeze
  Tgt-reg Nregs Init Code C Acc -> (kl-imp-mk-closure
                                    Tgt-reg [] Nregs Init Code C Acc))

(define kl-imp-freeze
  [] Nregs Init Code C Acc ->
  (kl-imp-return 1 (kl-imp-mk-freeze 1 Nregs Init Code C Acc))
  Tgt-reg Nregs Init Code C Acc ->
  (let Acc (kl-imp-mk-freeze Tgt-reg Nregs Init Code C Acc)
    [[klvm-stack-> (+ Tgt-reg 1) [klvm-reg Tgt-reg]] | Acc]))

(define kl-imp-expr2
  X _ _ C Acc <- (kl-imp-expr3 X C Acc)

  [shen-mk-closure Args Nregs Init Code] [] true C Acc ->
  (kl-imp-closure [] Args Nregs Init Code C Acc)

  [shen-mk-closure Args Nregs Init Code] Return-reg _ C Acc ->
  (kl-imp-closure Return-reg Args Nregs Init Code C Acc)

  [shen-mk-freeze Nregs Init Code] Return-reg _ C Acc ->
  (kl-imp-freeze Return-reg Nregs Init Code C Acc)

  [shen-mk-freeze Nregs Init Code] [] true C Acc ->
  (kl-imp-freeze [] Nregs Init Code C Acc)

  [klvm-current-error] Return-reg _ C Acc ->
  [[klvm-reg-> [Return-reg] [klvm-current-error]] | Acc]

  [klvm-error-unwind-get-handler] Return-reg _ C Acc ->
  [[klvm-reg-> [Return-reg] [klvm-error-unwind-get-handler]] | Acc]

  [F | Args] _ true C Acc -> (kl-imp-tailcall F Args C Acc)
  [F | Args] Return-reg false C Acc -> (kl-imp-call F Args Return-reg C Acc)
  X _ Tail? C Acc -> [X | Acc])

(define kl-imp-do
  [X] true C Acc -> (kl-imp-expr1 X true C Acc)
  [X] false C Acc -> (kl-imp-expr1 X false C Acc)
  [X | Rest] Tail? C Acc -> (let Acc' (kl-imp-expr1 X false C Acc)
                              (kl-imp-do Rest Tail? C Acc')))

(define kl-imp-if
  [[shen-get-reg R] Then Else] Tail? C Acc ->
  (let If-label (kl-imp-next-label C)
       Acc [[klvm-goto If-label] | Acc]
       Then-label (kl-imp-next-label C)
       Acc (kl-imp-expr1 Then Tail? C (kl-imp-label Then-label C Acc))
       Else-label (kl-imp-next-label C)
       Acc (kl-imp-expr1 Else Tail? C (kl-imp-label Else-label C Acc))
       R' (+ (impcontext-nargs C) R 1)
       Acc (kl-imp-label If-label C Acc)
    [[if [klvm-reg R'] [klvm-goto Then-label] [klvm-goto Else-label]] | Acc])
  X _ _ _ -> (error "Broken or unpreprocessed KLambda [~A]." X))

(define kl-imp-expr1
  [do | X] Tail? C Acc -> (kl-imp-do X Tail? C Acc)
  [if | X] Tail? C Acc -> (kl-imp-if X Tail? C Acc)
  [shen-get-reg R] true C Acc -> (let N (+ (impcontext-nargs C) R 1)
                                   (kl-imp-return N Acc))
  [shen-get-reg R] false C Acc -> (error "Nontail (shen-get-reg ~A)" R)
  [shen-set-reg! R Code] false C Acc -> (let N (+ (impcontext-nargs C) R 1)
                                          (kl-imp-expr2 Code N false C Acc))
  [shen-set-reg! R Code] true C Acc -> (error "Tail [shen-set-reg!]")
  [shen-get-arg R] true C Acc -> (kl-imp-return (+ R 1) Acc)
  [shen-get-arg R] false C Acc -> (error "Nontail (shen-get-arg).")

  [shen-mk-closure Args Nregs Init Code] true C Acc ->
  (kl-imp-closure [] Args Nregs Init Code C Acc)

  [shen-mk-closure Args Nregs Init Code] false _ Acc -> Acc

  [shen-mk-freeze Nregs Init Code] true C Acc ->
  (kl-imp-freeze Nregs Init Code C Acc)

  [shen-mk-freeze Nregs Init Code] false _ Acc -> Acc

  [klvm-push-error-handler E] _ C Acc ->
  [[klvm-push-error-handler | (kl-imp-expr3 E C [])] | Acc]

  [klvm-pop-error-handler] _ _ Acc -> [[klvm-pop-error-handler] | Acc]
  [F | Args] true C Acc -> (kl-imp-expr2 [F | Args] 1 true C Acc)
  [F | Args] false C Acc -> (kl-imp-expr2 [F | Args] [] false C Acc)
  [X | Y] _ _ _ -> (error "Broken or unpreprocessed KLambda ~A." [X | Y])
  X false _ Acc -> Acc
  X true _ Acc -> (kl-imp-return-val X Acc))

(define kl-imp-func-entry*
  C -> [klvm-nargs-cond
        [[klvm-nregs-> [2]]
         [klvm-reg-> [1] [klvm-func-obj]]
         [klvm-return]]
        [[klvm-dec-nargs (impcontext-nargs C)]]
        [[klvm-dec-nargs (impcontext-nargs C)]
         [klvm-stack-size [klvm-nargs]]
         [klvm-push-extra-args [klvm-nargs]]
         [klvm-inc-stack-ptr [klvm-nargs]]]])

(define kl-imp-func-entry
  C -> (let N (+ (impcontext-nargs C) (impcontext-nregs C) 2)
            Acc (kl-imp-label (kl-imp-next-label C) C [])
            Acc [(kl-imp-func-entry* C) | Acc]
            Acc [[klvm-nregs-> [N]] | Acc]
            Acc [[klvm-stack-size N] | Acc]
            Acc [[klvm-stack-> 0 [klvm-nargs]] | Acc]
         Acc))

(define kl-imp-toplevel-expr
  [shen-mk-func Name Args Nregs Code] Acc ->
  (let C (mk-impcontext (length Args) Nregs -1 [] Acc [] none)
       X (kl-imp-func-entry C)
       X (kl-imp-expr1 Code true C X)
       X (kl-imp-close-label C X)
       Acc (impcontext-toplevel C)
    [[shen-mk-func Name Args Nregs (reverse (impcontext-func C))] | Acc])
  [X] Acc -> [[klvm-call X] [klvm-nargs-> [0]] | Acc]
  X _ -> (error "Unexpected toplevel expression [~A]." X))

(define kl-imp-toplevel
  [] Acc -> (reverse Acc)
  [X | Y] Acc -> (kl-imp-toplevel Y (kl-imp-toplevel-expr X Acc)))

(define kl-imperative
  X -> (kl-imp-toplevel X []))

(define klvm-from-kl
  X -> (kl-imperative (reg-kl-walk (map (function kl-unwind) X))))

(define klvm-runtime
  -> (let X (intern "X")
          E (intern "E")
          R (intern "R")
      [
       [defun klvm-trap-error [X E]
         [do [klvm-push-error-handler E]
             [let R [X]
               [do [klvm-pop-error-handler]
                   R]]]]

       [defun klvm-call-error-handler []
         [let E [klvm-error-unwind-get-handler]
           [E [klvm-current-error]]]]
       ]))


\*

(reg-kl-walk (map (function kl-unwind) (klvm-runtime)))

[shen-mk-func klvm-trap-error [X E] 1
  [do [klvm-push-error-handler [shen-get-arg 1]]
      [shen-set-reg! 0 [[shen-get-arg 0]]]
      [klvm-pop-error-handler]
      [shen-get-reg 0]]]

[shen-mk-func klvm-call-error-handler [] 2
  [do [shen-set-reg! 0 [klvm-error-unwind-get-handler]]
      [shen-set-reg! 1 [klvm-current-error]]
      [[shen-get-reg 0] [shen-get-reg 1]]]]

*\
