(defstruct impcontext
  (nargs number)
  (nregs number)
  (label number)
  (func s-expr)
  (toplevel s-expr)
  (primitives (list symbol))
  (bind-funcs symbol)
  (native-hook (A --> impcontext --> A)))

(define prepend
  X Acc -> (append (reverse X) Acc))

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

(define kl-imp-push-stack-aux
  N N _ Acc -> Acc
  Except N Except Acc -> (kl-imp-push-stack-aux (+ Except 1) N Except Acc)
  I N Except Acc -> (let Acc [[klvm-stack-> (+ I 1) [klvm-reg I]] | Acc]
                      (kl-imp-push-stack-aux (+ I 1) N Except Acc)))

(define kl-imp-push-stack
  Except C Acc -> (let N (+ (impcontext-nargs C) (impcontext-nregs C) 1)
                    (kl-imp-push-stack-aux 0 N Except Acc)))

(define kl-imp-pop-stack-aux
  N N _ Acc -> Acc
  Except N Except Acc -> (kl-imp-pop-stack-aux (+ Except 1) N Except Acc)
  I N Except Acc -> (let Acc [[klvm-reg-> [I] [klvm-stack (+ I 1)]] | Acc]
                      (kl-imp-pop-stack-aux (+ I 1) N Except Acc)))

(define kl-imp-pop-stack
  Except C Acc -> (let N (+ (impcontext-nargs C) (impcontext-nregs C) 1)
                       Acc [[klvm-dec-stack-ptr (+ N 1)] | Acc]
                    (kl-imp-pop-stack-aux 0 N Except Acc)))

(define kl-imp-stack-ptr
  Op C Acc -> (let N (+ (impcontext-nargs C) (impcontext-nregs C) 2)
                (if (= N 0)
                    Acc
                    [[Op N] | Acc])))

(define kl-imp-inc-stack-ptr
  C Acc -> (kl-imp-stack-ptr klvm-inc-stack-ptr C Acc))

(define kl-imp-dec-stack-ptr
  C Acc -> (kl-imp-stack-ptr klvm-dec-stack-ptr C Acc))

(define kl-imp-set-native-arg
  _ _ X C _ -> (error "Unexpected arg expression ~R" X) where (= X (fail))
  Off I X C Acc -> [[klvm-reg-> [(+ I 1) | Off] X] | Acc])

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
                           (kl-imp-set-args Off (+ I 1) Y C Acc))
                         where (not (cons? X))
  _ _ [X | _] _ _ -> (error "Unexpected arg expression ~R" X))

(define kl-imp-call-func
  F Nargs true C Acc -> (prepend [[klvm-nargs-> Nargs] [klvm-call F]] Acc)
                        where (and (symbol? F)
                                   (or (= (impcontext-bind-funcs C) all)
                                       (and (= (impcontext-bind-funcs C) sys)
                                            (shen-sysfunc? F))))

  F Nargs false C Acc -> (prepend [[klvm-nargs-> Nargs] [klvm-call F]]
                                  (kl-imp-inc-stack-ptr C Acc))
                         where (symbol? F)

  F Nargs true C Acc -> []

  F Nargs false C Acc ->
  (let N-regs (+ (impcontext-nargs C) (impcontext-nregs C))
       Acc (kl-imp-inc-stack-ptr C Acc)
    (prepend [[klvm-nargs-> Nargs]
              [klvm-inc-nargs [klvm-closure-nargs]]
              [klvm-call F]]
             Acc)))

(define kl-imp-use-call-ret
  [] C Acc -> (kl-imp-pop-stack _ C Acc)
  Return-reg C Acc -> (let X [klvm-reg-> [Return-reg] [klvm-reg 1]]
                        (kl-imp-pop-stack Return-reg C [X | Acc])))

(define kl-imp-call
  F Args Return-reg C Acc ->
  (let Acc (kl-imp-push-stack _ C Acc)
       Acc [[klvm-nregs-> [(+ (length Args) 1)]] | Acc]
       Acc [[klvm-reg-> [0] (+ (impcontext-label C) 1)] | Acc]
       Acc (kl-imp-set-args [] 0 Args C Acc)
       X (if (cons? F)
             (kl-imp-expr3 F C)
             F)
       Acc (kl-imp-call-func X (length Args) false C Acc)
       Acc (kl-imp-label (kl-imp-next-label C) C Acc)
    (kl-imp-use-call-ret Return-reg C Acc))
  where (symbol? F)

  F Args Return-reg C Acc ->
  (let Acc (kl-imp-push-stack _ C Acc)
       X (kl-imp-expr3 F C)
       Acc (prepend [[klvm-closure-> X]
                     [klvm-nregs-> [(+ (length Args) 1) [klvm-closure-nargs]]]
                     [klvm-pop-closure-args X]
                     [klvm-reg-> [0] (+ (impcontext-label C) 1)]]
                    Acc)
       Acc (kl-imp-set-args [[klvm-closure-nargs]] 0 Args C Acc)
       Acc (kl-imp-call-func [klvm-closure-func] (length Args) false C Acc)
       Acc (kl-imp-label (kl-imp-next-label C) C Acc)
    (kl-imp-use-call-ret Return-reg C Acc)))

(define kl-imp-tailcall
  F Args C Acc -> (let N (length Args)
                       Acc (kl-imp-push-stack _ C Acc)
                       Acc (kl-imp-set-args [] 0 Args C Acc)
                    (prepend [[klvm-nargs-> [klvm-stack 0]]
                              [klvm-nregs-> [[klvm-nargs] N]]
                              [klvm-dec-stack-ptr [klvm-nargs]]
                              [klvm-pop-extra-args [klvm-nargs]]
                              [klvm-inc-nargs N]
                              [klvm-call F]]
                             Acc))
                  where (symbol? F)
  F Args C Acc -> (let N (length Args)
                       Acc (kl-imp-push-stack _ C Acc)
                       X (kl-imp-expr3 F C)
                       Acc (prepend [[klvm-nargs-> [klvm-stack 0]]
                                     [klvm-closure-> X]
                                     [klvm-nregs->
                                      [[klvm-nargs] N [klvm-closure-nargs]]]
                                     [klvm-pop-closure-args X]]
                                    Acc)
                       Acc (kl-imp-set-args
                            [[klvm-closure-nargs]] 0 Args C Acc)
                    (prepend [[klvm-dec-stack-ptr [klvm-nargs]]
                              [klvm-pop-extra-args [klvm-nargs]]
                              [klvm-inc-nargs N]
                              [klvm-inc-nargs [klvm-closure-nargs]]
                              [klvm-call [klvm-closure-func]]]
                             Acc)))

(define kl-imp-return-template
  X -> [klvm-nargs>0
        [[klvm-nregs-> [[klvm-nargs]]]
         [klvm-dec-stack-ptr [klvm-nargs]]
         [klvm-pop-extra-args [klvm-nargs]]
         [klvm-call X]]
        [[klvm-reg-> [1] X]
         [klvm-return]]])

(define kl-imp-return-val
  X C Acc -> (let Acc' [[klvm-nargs-> [klvm-stack 0]] | Acc]
               [(kl-imp-return-template X) | Acc']))

(define kl-imp-return
  Target-reg C Acc -> (kl-imp-return-val [klvm-reg Target-reg] C Acc))

(define kl-imp-closure-init-aux
  I [] C Acc -> Acc
  I [X | Init] C Acc -> (let Y [klvm-stack-> I (kl-imp-expr3 X C)]
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
       TL (kl-imp-toplevel-expr [shen-mk-func F A Nregs Code]
                                (impcontext-native-hook C)
                                TL)
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
    (kl-imp-return 1 C Acc))
  Tgt-reg Args Nregs Init Code C Acc ->
  (let Acc (kl-imp-mk-closure Tgt-reg Args Nregs Init Code C Acc)
    [[klvm-stack-> (+ Tgt-reg 1) [klvm-reg Tgt-reg]] | Acc]))

(define kl-imp-mk-freeze
  Tgt-reg Nregs Init Code C Acc -> (kl-imp-mk-closure
                                    Tgt-reg [] Nregs Init Code C Acc))

(define kl-imp-freeze
  [] Nregs Init Code C Acc ->
  (kl-imp-return 1 C (kl-imp-mk-freeze 1 Nregs Init Code C Acc))
  Tgt-reg Nregs Init Code C Acc ->
  (let Acc (kl-imp-mk-freeze Tgt-reg Nregs Init Code C Acc)
    [[klvm-stack-> (+ Tgt-reg 1) [klvm-reg Tgt-reg]] | Acc]))

(define kl-imp-expr3''
  [type X Type] C -> [type (kl-imp-expr3 X C) Type]
  [shen-get-reg R] C -> [klvm-reg (+ (impcontext-nargs C) R 1)]
  [shen-get-arg R] C -> [klvm-reg (+ R 1)]
  X _ -> (fail) where (cons? X)
  X _ -> X)

(define kl-imp-expr3'
  X C <- (kl-imp-expr3'' X C)
  X C <- ((impcontext-native-hook C) (/. X (kl-imp-expr3'' X C)) X)
         where (cons? X)
  _ _ -> (fail))

(define kl-imp-expr3
  X C <- (kl-imp-expr3' X C)
  X _ -> (error "Unexpected L3 expression ~S" X))

(define kl-imp-expr2''
  X _ _ _ _ -> (fail) where (= X (fail))
  X _ true C Acc -> (kl-imp-return-val X C Acc)
  X [] _ _ Acc -> [X | Acc]
  X Return-reg false C Acc -> [[klvm-reg-> [Return-reg] X] | Acc])

(define kl-imp-expr2'
  X Return-reg Tail? C Acc -> (let X' (kl-imp-expr3' X C)
                                (kl-imp-expr2'' X' Return-reg Tail? C Acc)))

(define kl-imp-expr2
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

  X Return-reg Tail? C Acc <- (kl-imp-expr2' X Return-reg Tail? C Acc)
  [F | Args] _ true C Acc -> (kl-imp-tailcall F Args C Acc)
  [F | Args] Return-reg false C Acc -> (kl-imp-call F Args Return-reg C Acc)
  X _ _ _ _ -> (error "Unexpected L2 expression ~S" X))

(define kl-imp-do
  [X] true C Acc -> (kl-imp-expr1 X true C Acc)
  [X] false C Acc -> (kl-imp-expr1 X false C Acc)
  [X | Rest] Tail? C Acc -> (let Acc' (kl-imp-expr1 X false C Acc)
                              (kl-imp-do Rest Tail? C Acc')))

(define kl-imp-if-expr
  X X-label _ true C Acc -> (let L (kl-imp-label X-label C Acc)
                              (kl-imp-expr1 X true C L))
  X X-label After-label false C Acc -> (let L (kl-imp-label X-label C Acc)
                                            Acc (kl-imp-expr1 X false C L)
                                         [[klvm-goto After-label] | Acc]))

(define kl-imp-if
  [[shen-get-reg R] Then Else] Tail? C Acc ->
  (let If-label (kl-imp-next-label C)
       After-label (kl-imp-next-label C)
       Acc [[klvm-goto If-label] | Acc]
       Then-label (kl-imp-next-label C)
       Acc (kl-imp-if-expr Then Then-label After-label Tail? C Acc)
       Else-label (kl-imp-next-label C)
       Acc (kl-imp-if-expr Else Else-label After-label Tail? C Acc)
       R' (+ (impcontext-nargs C) R 1)
       Acc (kl-imp-label If-label C Acc)
       X [if [klvm-reg R'] [klvm-goto Then-label] [klvm-goto Else-label]]
       Acc [X | Acc]
    (if Tail?
        Acc
        (kl-imp-label After-label C Acc)))
  X _ _ _ -> (error "Broken or unpreprocessed KLambda ~S." X))

(define kl-imp-expr1
  [do | X] Tail? C Acc -> (kl-imp-do X Tail? C Acc)
  [if | X] Tail? C Acc -> (kl-imp-if X Tail? C Acc)
  [shen-get-reg R] true C Acc -> (let N (+ (impcontext-nargs C) R 1)
                                   (kl-imp-return N C Acc))
  [shen-get-reg R] false C Acc -> Acc
  [shen-set-reg! R Code] false C Acc -> (let N (+ (impcontext-nargs C) R 1)
                                          (kl-imp-expr2 Code N false C Acc))
  [shen-set-reg! R Code] true C Acc -> Acc
  [shen-get-arg R] true C Acc -> (kl-imp-return (+ R 1) C Acc)
  [shen-get-arg R] false C Acc -> Acc

  [shen-mk-closure Args Nregs Init Code] true C Acc ->
  (kl-imp-closure [] Args Nregs Init Code C Acc)

  [shen-mk-closure Args Nregs Init Code] false _ Acc -> Acc

  [shen-mk-freeze Nregs Init Code] true C Acc ->
  (kl-imp-freeze Nregs Init Code C Acc)

  [shen-mk-freeze Nregs Init Code] false _ Acc -> Acc

  [klvm-push-error-handler E] _ C Acc ->
  [[klvm-push-error-handler (kl-imp-expr3 E C)] | Acc]

  [klvm-pop-error-handler] _ _ Acc -> [[klvm-pop-error-handler] | Acc]
  [F | Args] true C Acc -> (kl-imp-expr2 [F | Args] 1 true C Acc)
  [F | Args] false C Acc -> (kl-imp-expr2 [F | Args] [] false C Acc)
  [X | Y] _ _ _ -> (error "Broken or unpreprocessed KLambda ~S." [X | Y])
  X false _ Acc -> Acc
  X true C Acc -> (kl-imp-return-val (kl-imp-expr3 X C) C Acc))

(define kl-imp-func-entry-template
  Nargs -> [klvm-nargs-cond
            [[klvm-nregs-> [2]]
             [klvm-reg-> [1] [klvm-func-obj]]
             [klvm-return]]
            [[klvm-dec-nargs Nargs]]
            [[klvm-dec-nargs Nargs]
             [klvm-stack-size [klvm-nargs]]
             [klvm-push-extra-args [klvm-nargs]]
             [klvm-inc-stack-ptr [klvm-nargs]]]])

(define kl-imp-func-entry
  C -> (let N (+ (impcontext-nargs C) (impcontext-nregs C) 2)
            Acc (kl-imp-label (kl-imp-next-label C) C [])
         (prepend [(kl-imp-func-entry-template (impcontext-nargs C))
                   [klvm-nregs-> [N]]
                   [klvm-stack-size N]
                   [klvm-stack-> 0 [klvm-nargs]]]
                  Acc)))

(define kl-imp-toplevel-expr
  [shen-mk-func Name Args Nregs Code] F Acc ->
  (let C (mk-impcontext (length Args) Nregs -1 [] Acc [] none F)
       X (kl-imp-func-entry C)
       X (kl-imp-expr1 Code true C X)
       X (kl-imp-close-label C X)
       Acc (impcontext-toplevel C)
    [[shen-mk-func Name Args Nregs (reverse (impcontext-func C))] | Acc])
  [X] _ Acc -> [[klvm-call X] [klvm-nargs-> [0]] | Acc]
  X _ _ -> (error "Unexpected toplevel expression ~S." X))

(define kl-imp-toplevel
  [] _ Acc -> (reverse Acc)
  [X | Y] F Acc -> (kl-imp-toplevel Y F (kl-imp-toplevel-expr X F Acc)))

(define klvm-null-fn
  Y X <- (do (output "null: ~S~%" X) (fail))
  _ _ -> (fail))

(define klvm-from-kl
  F X -> (kl-imp-toplevel (reg-kl-walk (map (function kl-unwind) X)) F []))

(define kl-imp-template-func-body
  Nargs-sym Func-sym -> (let X [(kl-imp-func-entry-template Nargs-sym)
                                (kl-imp-return-template [Func-sym])]
                          [[[klvm-label 0] | X]]))

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

       [defun klvm-thaw [X]
         [X]]
       ]))

(define klvm-dump
  Srcdir F Dstdir -> (let D (make-string "~A~A.klvm" Dstdir F)
                          S (make-string "~A~A" Srcdir F)
                          Kl (map (function kl-from-shen) (read-file S))
                          _ (if (= (value shen-*hush*) hushed)
                                _
                                (output "== ~A -> ~A~%" S D))
                       (backend-write-file (klvm-from-kl Kl) D)))

(declare klvm-dump [string --> [string --> [string --> boolean]]])
(declare dump-to-file [string --> [string --> boolean]])

(if (trap-error (do (register-dumper) true) (/. _ false))
    (register-dumper klvm all klvm-dump)
    _)

(define kl-imp-for-each
  _ [] -> []
  F [X | List] -> (do (F X)
                      (kl-imp-for-each F List)))

(define kl-imp-show-label-code-aux
  _ [] -> (do (output "]~%") true)
  Sep [X | Y] -> (do (output "~A~S" Sep X)
                     (kl-imp-show-label-code-aux (make-string "~%     ") Y)))

(define kl-imp-show-label-code
  X -> (do (output "    [")
           (kl-imp-show-label-code-aux "" X)))

(define kl-imp-show-code
  [] -> (do (output "~%")
            true)
  [[shen-mk-func Name Args Nregs Code] | Y] ->
  (let . (output "  [shen-mk-func ~S ~S ~S~%" Name Args Nregs)
       . (kl-imp-for-each (function kl-imp-show-label-code) Code)
       . (output "  ]~%")
    (kl-imp-show-code Y))
  [X | Y] -> (let S (value *maximum-print-sequence-size*)
                  . (set *maximum-print-sequence-size* -1)
                  T1 (output "  ~S~%" X)
                  . (set *maximum-print-sequence-size* S)
                 (kl-imp-show-code Y)))
