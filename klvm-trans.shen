(package klvm-trans [deinline-expr.deinline klvm-dump klvm-from-kl
                     reg-kl.walk
                     shen-get-arg shen-get-reg shen-set-reg! shen-closure
                     shen-func shen-toplevel shen-freeze

                     klvm-call
                     klvm-closure->
                     klvm-closure-func
                     klvm-closure-nargs
                     klvm-current-error
                     klvm-dec-nargs
                     klvm-dec-stack-ptr
                     klvm-error-unwind-get-handler
                     klvm-func-obj
                     klvm-goto
                     klvm-inc-nargs
                     klvm-inc-stack-ptr
                     klvm-label
                     klvm-mk-closure
                     klvm-nargs
                     klvm-nargs->
                     klvm-nargs>0
                     klvm-nargs-cond
                     klvm-nregs->
                     klvm-pop-closure-args
                     klvm-pop-error-handler
                     klvm-pop-extra-args
                     klvm-push-error-handler
                     klvm-push-extra-args
                     klvm-reg
                     klvm-reg->
                     klvm-return
                     klvm-stack
                     klvm-stack->
                     klvm-stack-size
                     klvm-thaw
                     klvm-closure
                     klvm-func
                     klvm-toplevel]

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

(define next-label
  C -> (do (impcontext-label-> C (+ (impcontext-label C) 1))
           (impcontext-label C)))

(define close-label
  C [] -> []
  C Acc -> (do (impcontext-func-> C [(reverse Acc) | (impcontext-func C)])
               []))

(define label
  N C Acc -> (do (close-label C Acc)
                 [[klvm-label N]]))

(define push-stack-aux
  N N _ Acc -> Acc
  Except N Except Acc -> (push-stack-aux (+ Except 1) N Except Acc)
  I N Except Acc -> (let Acc [[klvm-stack-> (+ I 1) [klvm-reg I]] | Acc]
                      (push-stack-aux (+ I 1) N Except Acc)))

(define push-stack
  Except C Acc -> (let N (+ (impcontext-nargs C) (impcontext-nregs C) 1)
                    (push-stack-aux 0 N Except Acc)))

(define pop-stack-aux
  N N _ Acc -> Acc
  Except N Except Acc -> (pop-stack-aux (+ Except 1) N Except Acc)
  I N Except Acc -> (let Acc [[klvm-reg-> [I] [klvm-stack (+ I 1)]] | Acc]
                      (pop-stack-aux (+ I 1) N Except Acc)))

(define pop-stack
  Except C Acc -> (let N (+ (impcontext-nargs C) (impcontext-nregs C) 1)
                       Acc [[klvm-dec-stack-ptr (+ N 1)] | Acc]
                    (pop-stack-aux 0 N Except Acc)))

(define stack-ptr
  Op C Acc -> (let N (+ (impcontext-nargs C) (impcontext-nregs C) 2)
                (if (= N 0)
                    Acc
                    [[Op N] | Acc])))

(define inc-stack-ptr
  C Acc -> (stack-ptr klvm-inc-stack-ptr C Acc))

(define dec-stack-ptr
  C Acc -> (stack-ptr klvm-dec-stack-ptr C Acc))

(define imp-set-native-arg
  _ _ X C _ -> (error "Unexpected arg expression ~R" X) where (= X (fail))
  Off I X C Acc -> [[klvm-reg-> [(+ I 1) | Off] X] | Acc])

(define imp-set-args
  Off _ [] _ Acc -> Acc

  Off I [[shen-get-arg X] | Y] C Acc ->
  (let Acc [[klvm-reg-> [(+ I 1) | Off] [klvm-stack (+ X 2)]] | Acc]
    (imp-set-args Off (+ I 1) Y C Acc))

  Off I [[shen-get-reg X] | Y] C Acc ->
  (let X' (+ (impcontext-nargs C) X 2)
       Acc [[klvm-reg-> [(+ I 1) | Off] [klvm-stack X']] | Acc]
    (imp-set-args Off (+ I 1) Y C Acc))

  Off I [X | Y] C Acc -> (let Acc [[klvm-reg-> [(+ I 1) | Off] X] | Acc]
                           (imp-set-args Off (+ I 1) Y C Acc))
                         where (not (cons? X))
  _ _ [X | _] _ _ -> (error "Unexpected arg expression ~R" X))

(define imp-call-func
  F Nargs true C Acc -> (prepend [[klvm-nargs-> Nargs] [klvm-call F]] Acc)
                        where (and (symbol? F)
                                   (or (= (impcontext-bind-funcs C) all)
                                       (and (= (impcontext-bind-funcs C) sys)
                                            (shen.sysfunc? F))))

  F Nargs false C Acc -> (prepend [[klvm-nargs-> Nargs] [klvm-call F]]
                                  (inc-stack-ptr C Acc))
                         where (and (symbol? F)
                                    (= (impcontext-bind-funcs C) all))

  F Nargs true C Acc -> []

  F Nargs false C Acc ->
  (let N-regs (+ (impcontext-nargs C) (impcontext-nregs C))
       Acc (inc-stack-ptr C Acc)
    (prepend [[klvm-nargs-> Nargs]
              [klvm-inc-nargs [klvm-closure-nargs]]
              [klvm-call F]]
             Acc)))

(define imp-use-call-ret
  [] C Acc -> (pop-stack _ C Acc)
  Return-reg C Acc -> (let X [klvm-reg-> [Return-reg] [klvm-reg 1]]
                        (pop-stack Return-reg C [X | Acc])))

(define imp-call
  F Args Return-reg C Acc ->
  (let Acc (push-stack _ C Acc)
       Acc [[klvm-nregs-> [(+ (length Args) 1)]] | Acc]
       Acc [[klvm-reg-> [0] (+ (impcontext-label C) 1)] | Acc]
       Acc (imp-set-args [] 0 Args C Acc)
       X (if (cons? F)
             (imp-expr3 F C)
             F)
       Acc (imp-call-func X (length Args) false C Acc)
       Acc (label (next-label C) C Acc)
    (imp-use-call-ret Return-reg C Acc))
  where (and (symbol? F) (= (impcontext-bind-funcs C) all))

  F Args Return-reg C Acc ->
  (let Acc (push-stack _ C Acc)
       X (imp-expr3 F C)
       Acc (prepend [[klvm-closure-> X]
                     [klvm-nregs-> [(+ (length Args) 1)
                                    [klvm-closure-nargs]]]
                     [klvm-pop-closure-args]
                     [klvm-reg-> [0] (+ (impcontext-label C) 1)]]
                    Acc)
       Acc (imp-set-args [[klvm-closure-nargs]] 0 Args C Acc)
       Acc (imp-call-func [klvm-closure-func] (length Args) false C Acc)
       Acc (label (next-label C) C Acc)
    (imp-use-call-ret Return-reg C Acc)))

(define imp-tailcall
  F Args C Acc -> (let N (length Args)
                       Acc (push-stack _ C Acc)
                       Acc (imp-set-args [] 0 Args C Acc)
                    (prepend [[klvm-nargs-> [klvm-stack 0]]
                              [klvm-nregs-> [[klvm-nargs] N]]
                              [klvm-dec-stack-ptr [klvm-nargs]]
                              [klvm-pop-extra-args [klvm-nargs]]
                              [klvm-inc-nargs N]
                              [klvm-call F]]
                             Acc))
                  where (and (symbol? F) (= (impcontext-bind-funcs C) all))
  F Args C Acc -> (let N (length Args)
                       Acc (push-stack _ C Acc)
                       X (imp-expr3 F C)
                       Acc (prepend [[klvm-nargs-> [klvm-stack 0]]
                                     [klvm-closure-> X]
                                     [klvm-nregs->
                                      [[klvm-nargs] N [klvm-closure-nargs]]]
                                     [klvm-pop-closure-args]]
                                    Acc)
                       Acc (imp-set-args [[klvm-closure-nargs]] 0 Args C Acc)
                    (prepend [[klvm-dec-stack-ptr [klvm-nargs]]
                              [klvm-pop-extra-args [klvm-nargs]]
                              [klvm-inc-nargs N]
                              [klvm-inc-nargs [klvm-closure-nargs]]
                              [klvm-call [klvm-closure-func]]]
                             Acc)))

(define imp-return-template
  X -> [klvm-nargs>0
        [[klvm-nregs-> [[klvm-nargs]]]
         [klvm-dec-stack-ptr [klvm-nargs]]
         [klvm-pop-extra-args [klvm-nargs]]
         [klvm-call X]]
        [[klvm-reg-> [1] X]
         [klvm-return]]])

(define imp-return-val
  X C Acc -> (let Acc' [[klvm-nargs-> [klvm-stack 0]] | Acc]
               [(imp-return-template X) | Acc']))

(define imp-return
  Target-reg C Acc -> (imp-return-val [klvm-reg Target-reg] C Acc))

(define closure-init-aux
  I [] C Acc -> Acc
  I [X | Init] C Acc -> (let Y [klvm-stack-> I (imp-expr3 X C)]
                          (closure-init-aux (+ I 1) Init C [Y | Acc])))

(define closure-init
  [] _ Acc -> Acc
  Init C Acc -> (let Acc (inc-stack-ptr C Acc)
                     Acc [[klvm-stack-size (length Init)] | Acc]
                     Acc (closure-init-aux 0 Init C Acc)
                  Acc))

(define closure-args
  S N N Acc -> (reverse Acc)
  S I N Acc -> (closure-args S (+ I 1) N [(concat S I) | Acc]))

(define imp-mk-closure
  Tgt-reg Args Nregs Init Code C Acc ->
  (let Ninit (length Init)
       Nargs (+ Ninit (length Args))
       F (gensym klvm-lambda)
       TL (impcontext-toplevel C)
       A (closure-args (protect A) 0 Nargs [])
       TL (imp-toplevel-expr [shen-closure F A Nregs Code]
                                (impcontext-native-hook C)
                                TL)
       _ (impcontext-toplevel-> C TL)
       Acc (closure-init Init C Acc)
       X [klvm-reg-> [Tgt-reg] [klvm-mk-closure F Nargs Ninit]]
       Acc [X | Acc]
    (if (= Ninit 0)
        Acc
        (dec-stack-ptr C Acc))))

(define closure
  [] Args Nregs Init Code C Acc ->
  (let Acc (imp-mk-closure 1 Args Nregs Init Code C Acc)
    (imp-return 1 C Acc))
  Tgt-reg Args Nregs Init Code C Acc ->
  (let Acc (imp-mk-closure Tgt-reg Args Nregs Init Code C Acc)
    [[klvm-stack-> (+ Tgt-reg 1) [klvm-reg Tgt-reg]] | Acc]))

(define imp-mk-freeze
  Tgt-reg Nregs Init Code C Acc -> (imp-mk-closure
                                    Tgt-reg [] Nregs Init Code C Acc))

(define imp-freeze
  [] Nregs Init Code C Acc ->
  (imp-return 1 C (imp-mk-freeze 1 Nregs Init Code C Acc))
  Tgt-reg Nregs Init Code C Acc ->
  (let Acc (imp-mk-freeze Tgt-reg Nregs Init Code C Acc)
    [[klvm-stack-> (+ Tgt-reg 1) [klvm-reg Tgt-reg]] | Acc]))

(define imp-expr3''
  [type X Type] C -> [type (imp-expr3 X C) Type]
  [shen-get-reg R] C -> [klvm-reg (+ (impcontext-nargs C) R 1)]
  [shen-get-arg R] C -> [klvm-reg (+ R 1)]
  X _ -> (fail) where (cons? X)
  X _ -> X)

(define imp-expr3'
  X C <- (imp-expr3'' X C)
  X C <- ((impcontext-native-hook C) (/. X (imp-expr3'' X C)) X)
         where (cons? X)
  _ _ -> (fail))

(define imp-expr3
  X C <- (imp-expr3' X C)
  X _ -> (error "Unexpected L3 expression ~S" X))

(define imp-expr2''
  X _ _ _ _ -> (fail) where (= X (fail))
  X _ true C Acc -> (imp-return-val X C Acc)
  X [] _ _ Acc -> [X | Acc]
  X Return-reg false C Acc -> [[klvm-reg-> [Return-reg] X] | Acc])

(define imp-expr2'
  X Return-reg Tail? C Acc -> (let X' (imp-expr3' X C)
                                (imp-expr2'' X' Return-reg Tail? C Acc)))

(define imp-expr2
  [shen-closure Args Nregs Init Code] [] true C Acc ->
  (closure [] Args Nregs Init Code C Acc)

  [shen-closure Args Nregs Init Code] Return-reg _ C Acc ->
  (closure Return-reg Args Nregs Init Code C Acc)

  [shen-freeze Nregs Init Code] Return-reg _ C Acc ->
  (imp-freeze Return-reg Nregs Init Code C Acc)

  [shen-freeze Nregs Init Code] [] true C Acc ->
  (imp-freeze [] Nregs Init Code C Acc)

  [klvm-current-error] Return-reg _ C Acc ->
  [[klvm-reg-> [Return-reg] [klvm-current-error]] | Acc]

  [klvm-error-unwind-get-handler] Return-reg _ C Acc ->
  [[klvm-reg-> [Return-reg] [klvm-error-unwind-get-handler]] | Acc]

  X Return-reg Tail? C Acc <- (imp-expr2' X Return-reg Tail? C Acc)
  [F | Args] _ true C Acc -> (imp-tailcall F Args C Acc)
  [F | Args] Return-reg false C Acc -> (imp-call F Args Return-reg C Acc)
  X _ _ _ _ -> (error "Unexpected L2 expression ~S" X))

(define imp-do
  [X] true C Acc -> (imp-expr1 X true C Acc)
  [X] false C Acc -> (imp-expr1 X false C Acc)
  [X | Rest] Tail? C Acc -> (let Acc' (imp-expr1 X false C Acc)
                              (imp-do Rest Tail? C Acc')))

(define imp-if-expr
  X X-label _ true C Acc -> (let L (label X-label C Acc)
                              (imp-expr1 X true C L))
  X X-label After-label false C Acc -> (let L (label X-label C Acc)
                                            Acc (imp-expr1 X false C L)
                                         [[klvm-goto After-label] | Acc]))

(define imp-if
  [[shen-get-reg R] Then Else] Tail? C Acc ->
  (let If-label (next-label C)
       After-label (next-label C)
       Acc [[klvm-goto If-label] | Acc]
       Then-label (next-label C)
       Acc (imp-if-expr Then Then-label After-label Tail? C Acc)
       Else-label (next-label C)
       Acc (imp-if-expr Else Else-label After-label Tail? C Acc)
       R' (+ (impcontext-nargs C) R 1)
       Acc (label If-label C Acc)
       X [if [klvm-reg R'] [klvm-goto Then-label] [klvm-goto Else-label]]
       Acc [X | Acc]
    (if Tail?
        Acc
        (label After-label C Acc)))
  X _ _ _ -> (error "Broken or unpreprocessed KLambda ~S." X))

(define imp-expr1
  [do | X] Tail? C Acc -> (imp-do X Tail? C Acc)
  [if | X] Tail? C Acc -> (imp-if X Tail? C Acc)
  [shen-get-reg R] true C Acc -> (let N (+ (impcontext-nargs C) R 1)
                                   (imp-return N C Acc))
  [shen-get-reg R] false C Acc -> Acc
  [shen-set-reg! R Code] false C Acc -> (let N (+ (impcontext-nargs C) R 1)
                                          (imp-expr2 Code N false C Acc))
  [shen-set-reg! R Code] true C Acc -> Acc
  [shen-get-arg R] true C Acc -> (imp-return (+ R 1) C Acc)
  [shen-get-arg R] false C Acc -> Acc

  [shen-closure Args Nregs Init Code] true C Acc ->
  (closure [] Args Nregs Init Code C Acc)

  [shen-closure Args Nregs Init Code] false _ Acc -> Acc

  [shen-freeze Nregs Init Code] true C Acc ->
  (imp-freeze Nregs Init Code C Acc)

  [shen-freeze Nregs Init Code] false _ Acc -> Acc

  [klvm-push-error-handler E] _ C Acc ->
  [[klvm-push-error-handler (imp-expr3 E C)] | Acc]

  [klvm-pop-error-handler] _ _ Acc -> [[klvm-pop-error-handler] | Acc]
  [F | Args] true C Acc -> (imp-expr2 [F | Args] 1 true C Acc)
  [F | Args] false C Acc -> (imp-expr2 [F | Args] [] false C Acc)
  [X | Y] _ _ _ -> (error "Broken or unpreprocessed KLambda ~S." [X | Y])
  X false _ Acc -> Acc
  X true C Acc -> (imp-return-val (imp-expr3 X C) C Acc))

(define imp-func-entry-template
  Nargs -> [klvm-nargs-cond
            [[klvm-nregs-> [2]]
             [klvm-reg-> [1] [klvm-func-obj]]
             [klvm-return]]
            [[klvm-dec-nargs Nargs]]
            [[klvm-dec-nargs Nargs]
             [klvm-stack-size [klvm-nargs]]
             [klvm-push-extra-args [klvm-nargs]]
             [klvm-inc-stack-ptr [klvm-nargs]]]])

(define imp-func-entry
  C -> (let N (+ (impcontext-nargs C) (impcontext-nregs C) 2)
            Acc (label (next-label C) C [])
         (prepend [(imp-func-entry-template (impcontext-nargs C))
                   [klvm-nregs-> [N]]
                   [klvm-stack-size N]
                   [klvm-stack-> 0 [klvm-nargs]]]
                  Acc)))

(define imp-func-hdr
  shen-func -> klvm-func
  shen-closure -> klvm-closure
  shen-toplevel -> klvm-toplevel)

(define imp-toplevel-expr
  [Head Name Args Nregs Code] F Acc ->
  (let C (mk-impcontext (length Args) Nregs -1 [] Acc [] none F)
       X (imp-func-entry C)
       X (imp-expr1 Code true C X)
       X (close-label C X)
       Acc (impcontext-toplevel C)
       H (imp-func-hdr Head)
    [[H Name Args Nregs (reverse (impcontext-func C))] | Acc])
  where (element? Head [shen-func shen-toplevel shen-closure])
  [X | Y] _ _ -> (error "Unexpected toplevel expression ~S." [X | Y])
  X _ Acc -> [X | Acc])

(define imp-toplevel
  [] _ Acc -> (reverse Acc)
  [X | Y] F Acc -> (imp-toplevel Y F (imp-toplevel-expr X F Acc)))

(define null-fn
  _ _ -> (fail))

(define klvm-from-kl
  F X -> (let X' (reg-kl.walk (map (function deinline-expr.deinline) X) false)
           (imp-toplevel X' F [])))

(define imp-template-func-body
  Nargs-sym Func-sym -> (let X [(imp-func-entry-template Nargs-sym)
                                (imp-return-template [Func-sym])]
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
                          _ (if (value *hush*)
                                _
                                (output "** ~A -> ~A~%" S D))
                          X (klvm-from-kl (function null-fn) Kl)
                       (backend-utils.write-file X D)))

(declare klvm-dump [string --> [string --> [string --> boolean]]])

(if (trap-error (do (register-dumper) true) (/. _ false))
    (register-dumper klvm all klvm-dump)
    _)

(define for-each
  _ [] -> []
  F [X | List] -> (let . (F X)
                    (for-each F List)))

(define show-label-code-aux
  _ [] -> (do (output "]~%") true)
  Sep [X | Y] -> (do (output "~A~S" Sep X)
                     (show-label-code-aux (make-string "~%     ") Y)))

(define show-label-code
  X -> (do (output "    [")
           (show-label-code-aux "" X)))

(define show-code
  [] -> (do (output "~%")
            true)
  [[Head Name Args Nregs Code] | Y] ->
  (let . (output "  [~A ~S ~S ~S~%" Head Name Args Nregs)
       . (for-each (function show-label-code) Code)
       . (output "  ]~%")
    (show-code Y))
  where (element? Head [klvm-func klvm-toplevel klvm-closure])
  [X | Y] -> (let S (value *maximum-print-sequence-size*)
                  . (set *maximum-print-sequence-size* -1)
                  T1 (output "  ~S~%" X)
                  . (set *maximum-print-sequence-size* S)
                 (show-code Y)))
)
