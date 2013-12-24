(package klvm-trans [deinline-expr.deinline klvm-dump klvm-from-kl
                     reg-kl.walk
                     shen-get-arg shen-get-reg shen-set-reg! shen-closure
                     shen-func shen-toplevel shen-freeze
                     klvm-func-entry-tpl
                     klvm-func-return-tpl
                     klvm-runtime
                     klvm-trap-error
                     klvm-call-error-handler
                     klvm-lambda

                     klvm-call
                     klvm-closure->
                     klvm-closure-func
                     klvm-closure-nargs
                     klvm-current-error
                     klvm-dec-nargs
                     klvm-error-unwind-get-handler
                     klvm-func-obj
                     klvm-func-ptr
                     klvm-goto
                     klvm-if
                     klvm-inc-nargs
                     klvm-inc-stack-ptr
                     klvm-dec-stack-ptr
                     klvm-wipe-stack
                     klvm-label
                     klvm-mk-closure
                     klvm-next
                     klvm-next->
                     klvm-nargs
                     klvm-nargs->
                     klvm-if-nargs>0
                     klvm-nargs-cond
                     klvm-nregs->
                     klvm-put-closure-args
                     klvm-pop-error-handler
                     klvm-push-error-handler
                     klvm-reg
                     klvm-reg->
                     klvm-return
                     klvm-thaw
                     klvm-closure
                     klvm-func
                     klvm-toplevel]

(defstruct context
  (func-name symbol)
  (stack-size number)
  (nargs number)
  (nregs number)
  (label number)
  (func s-expr)
  (toplevel s-expr)
  (primitives (list symbol))
  (bind-funcs symbol)
  (native-hook (A --> context --> A)))

(define func-nargs-reg
  C -> (- (context-stack-size C) 2))

(define func-next-reg
  C -> (- (context-stack-size C) 1))

(define func-reg
  X C -> (+ (context-nargs C) X))

(define func-arg
  X C -> (- (context-nargs C) (+ X 1)))

(define warning
  X -> (output "Warning: ~A" X))

(define prepend
  X Acc -> (append (reverse X) Acc))

(define next-label
  C -> (do (context-label-> C (+ (context-label C) 1))
           (context-label C)))

(define close-label
  C [] -> []
  C Acc -> (do (context-func-> C [(reverse Acc) | (context-func C)])
               []))

(define label
  N C Acc -> (do (close-label C Acc)
                 [[klvm-label N]]))

\*

Y Y X X | X X X C C C Z Z Z
->
Y Y X X | X X X C C C _ _ _

Y Y X X | R X X X X
->
Y Y X X | R _ _ _ _

*\

(define klvm-func-entry-tpl
  Name Nargs -> [klvm-nargs-cond
                 Nargs
                 [[klvm-nregs-> [1]]
                  [klvm-reg-> 0 [klvm-func-obj Name Nargs]]
                  [klvm-wipe-stack 1]
                  [klvm-return [klvm-next]]]
                 [[klvm-dec-nargs Nargs]]
                 [[klvm-inc-stack-ptr [klvm-nargs]]
                  [klvm-dec-stack-ptr Nargs]
                  [klvm-dec-nargs Nargs]]])

(define klvm-func-return-tpl
  X Next -> [klvm-if-nargs>0
             X
             Next
             [[klvm-closure-> X]
              [klvm-nregs-> [[klvm-nargs] [klvm-closure-nargs]]]
              [klvm-next-> Next]
              [klvm-wipe-stack 0]
              [klvm-put-closure-args 0]
              [klvm-dec-stack-ptr [klvm-nargs]]
              [klvm-call [klvm-closure-func]]]
             [[klvm-reg-> 0 X]
              [klvm-next-> Next]
              [klvm-wipe-stack 1]
              [klvm-return [klvm-next]]]])

(define emit-return-val
  X C Acc -> (let Acc [[klvm-nargs-> [klvm-reg (func-nargs-reg C)]] | Acc]
               [(klvm-func-return-tpl X [klvm-reg (func-next-reg C)]) | Acc]))

(define emit-return
  Target-reg C Acc -> (emit-return-val [klvm-reg Target-reg] C Acc))

(define closure-args
  S N N Acc -> (reverse Acc)
  S I N Acc -> (closure-args S (+ I 1) N [(concat S I) | Acc]))

(define prep-args
  Args C -> (map (/. X (emit-expr3 X C)) Args))

(define closure
  Tgt-reg Args Nregs Init Code C Acc ->
  (let Ninit (length Init)
       Nargs (+ Ninit (length Args))
       F (gensym klvm-lambda)
       TL (context-toplevel C)
       A (closure-args (protect A) 0 Nargs [])
       TL (emit-toplevel-expr [shen-closure F A Nregs Code]
                              (context-native-hook C)
                              TL)
       _ (context-toplevel-> C TL)
       Init' [[klvm-func-ptr F] | (prep-args Init C)]
       Acc (if (= Tgt-reg [])
               (emit-tailcall klvm-mk-closure Init' C Acc)
               (emit-call klvm-mk-closure Init' Tgt-reg C Acc))
    Acc))

(define emit-freeze
  Tgt-reg Nregs Init Code C Acc -> (closure Tgt-reg [] Nregs Init Code C Acc))

(define emit-expr3''
  [type X Type] C -> (do (warning "`type` expression is not supported yet")
                         (emit-expr3 X C))
  [shen-get-reg R] C -> [klvm-reg (func-reg R C)]
  [shen-get-arg R] C -> [klvm-reg (func-arg R C)]
  X _ -> (fail) where (cons? X)
  X _ -> X)

(define emit-expr3'
  X C <- (emit-expr3'' X C)
  X C <- ((context-native-hook C) (/. X (emit-expr3'' X C)) X)
         where (cons? X)
  _ _ -> (fail))

(define emit-expr3
  X C <- (emit-expr3' X C)
  X _ -> (error "Unexpected L3 expression ~S" X))

(define emit-expr2''
  X _ _ _ _ -> (fail) where (= X (fail))
  X _ true C Acc -> (emit-return-val X C Acc)
  X [] _ _ Acc -> [X | Acc]
  X Return-reg false C Acc -> [[klvm-reg-> Return-reg X] | Acc])

(define emit-expr2'
  X Return-reg Tail? C Acc -> (let X' (emit-expr3' X C)
                                (emit-expr2'' X' Return-reg Tail? C Acc)))

(define emit-expr2
  [shen-closure Args Nregs Init Code] [] true C Acc ->
  (closure [] Args Nregs Init Code C Acc)
  
  [shen-closure Args Nregs Init Code] Return-reg _ C Acc ->
  (closure Return-reg Args Nregs Init Code C Acc)
  
  [shen-freeze Nregs Init Code] Return-reg _ C Acc ->
  (emit-freeze Return-reg Nregs Init Code C Acc)
  
  [shen-freeze Nregs Init Code] [] true C Acc ->
  (emit-freeze [] Nregs Init Code C Acc)
  
  [klvm-current-error] Return-reg _ C Acc ->
  [[klvm-reg-> Return-reg [klvm-current-error]] | Acc]
  
  [klvm-error-unwind-get-handler] Return-reg _ C Acc ->
  [[klvm-reg-> Return-reg [klvm-error-unwind-get-handler]] | Acc]
  
  X Return-reg Tail? C Acc <- (emit-expr2' X Return-reg Tail? C Acc)
  [F | Args] _ true C Acc -> (emit-tailcall F (prep-args Args C) C Acc)
  [F | Args] Return-reg false C Acc -> (let Args' (prep-args Args C)
                                         (emit-call F Args' Return-reg C Acc))
  X _ _ _ _ -> (error "Unexpected L2 expression ~S" X))

(define emit-do
  [X] true C Acc -> (emit-expr1 X true C Acc)
  [X] false C Acc -> (emit-expr1 X false C Acc)
  [X | Rest] Tail? C Acc -> (let Acc' (emit-expr1 X false C Acc)
                              (emit-do Rest Tail? C Acc')))

(define emit-if-expr
  X X-label _ true C Acc -> (let L (label X-label C Acc)
                              (emit-expr1 X true C L))
  X X-label After-label false C Acc -> (let L (label X-label C Acc)
                                            Acc (emit-expr1 X false C L)
                                         [[klvm-goto After-label] | Acc]))

(define emit-if
  [[shen-get-reg R] Then Else] Tail? C Acc ->
  (let If-label (next-label C)
       After-label (next-label C)
       Acc [[klvm-goto If-label] | Acc]
       Then-label (next-label C)
       Acc (emit-if-expr Then Then-label After-label Tail? C Acc)
       Else-label (next-label C)
       Acc (emit-if-expr Else Else-label After-label Tail? C Acc)
       R' (func-reg R C)
       Acc (label If-label C Acc)
       X [klvm-if [klvm-reg R'] [klvm-goto Then-label] [klvm-goto Else-label]]
       Acc [X | Acc]
    (if Tail?
        Acc
        (label After-label C Acc)))
  X _ _ _ -> (error "Broken or unpreprocessed KLambda ~S." X))

(define emit-expr1
  [do | X] Tail? C Acc -> (emit-do X Tail? C Acc)
  [if | X] Tail? C Acc -> (emit-if X Tail? C Acc)
  [shen-get-reg R] true C Acc -> (emit-return (func-reg R C) C Acc)
  [shen-get-reg R] false C Acc -> Acc
  [shen-set-reg! R Code] false C Acc -> (let N (func-reg R C)
                                          (emit-expr2 Code N false C Acc))
  [shen-set-reg! R Code] true C Acc -> Acc
  [shen-get-arg R] true C Acc -> (emit-return (func-arg R C) C Acc)
  [shen-get-arg R] false C Acc -> Acc

  [shen-closure Args Nregs Init Code] true C Acc ->
  (closure [] Args Nregs Init Code C Acc)

  [shen-closure Args Nregs Init Code] false _ Acc -> Acc

  [shen-freeze Nregs Init Code] true C Acc ->
  (emit-freeze [] Nregs Init Code C Acc)

  [shen-freeze Nregs Init Code] false _ Acc -> Acc

  [klvm-push-error-handler E] _ C Acc ->
  [[klvm-push-error-handler (emit-expr3 E C)] | Acc]

  [klvm-pop-error-handler] _ _ Acc -> [[klvm-pop-error-handler] | Acc]
  [F | Args] true C Acc -> (emit-expr2 [F | Args] 0 true C Acc)
  [F | Args] false C Acc -> (emit-expr2 [F | Args] [] false C Acc)
  [X | Y] _ _ _ -> (error "Broken or unpreprocessed KLambda ~S." [X | Y])
  X false _ Acc -> Acc
  X true C Acc -> (emit-return-val (emit-expr3 X C) C Acc))

(define emit-func-entry
  C -> (let N (context-stack-size C)
            Acc (label (next-label C) C [])
         (prepend [(klvm-func-entry-tpl (context-func-name C)
                                        (context-nargs C))
                   [klvm-nregs-> [N]]
                   [klvm-reg-> (- N 2) [klvm-nargs]]
                   [klvm-reg-> (- N 1) [klvm-next]]]
                  Acc)))

(define emit-func-hdr
  shen-func -> klvm-func
  shen-closure -> klvm-closure
  shen-toplevel -> klvm-toplevel)

(define emit-toplevel-expr
  [Head Name Args Nregs Code] F Acc ->
  (let Nargs (length Args)
       C (mk-context Name (+ Nargs Nregs 2) Nargs Nregs -1 [] Acc [] none F)
       X (emit-func-entry C)
       X (emit-expr1 Code true C X)
       X (close-label C X)
       Acc (context-toplevel C)
       H (emit-func-hdr Head)
    [[H Name Args Nregs (reverse (context-func C))] | Acc])
  where (element? Head [shen-func shen-toplevel shen-closure])
  [X | Y] _ _ -> (error "Unexpected toplevel expression ~S." [X | Y])
  X _ Acc -> [X | Acc])

(define emit-toplevel
  [] _ Acc -> (reverse Acc)
  [X | Y] F Acc -> (emit-toplevel Y F (emit-toplevel-expr X F Acc)))

(define null-fn
  _ _ -> (fail))

(define klvm-from-kl
  F X -> (let X' (reg-kl.walk (map (function deinline-expr.deinline) X) false)
           (emit-toplevel X' F [])))

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
    (register-dumper klvm s-expr klvm-dump)
    _)

(define for-each
  _ [] -> []
  F [X | List] -> (let . (F X)
                    (for-each F List)))

(define pretty-print-label-code-aux
  _ [] Out -> (do (pr (make-string ")~%") Out)
                  true)
  Sep [X | Y] Out -> (do (pr (make-string "~A~R" Sep X) Out)
                         (pretty-print-label-code-aux
                          (make-string "~%     ") Y Out)))

(define pretty-print-label-code
  Out X -> (do (pr (make-string "    (") Out)
               (pretty-print-label-code-aux "" X Out)))

(define pretty-print-code
  [] Out -> (do (pr "c#10;" Out)
                true)
  [[Head Name Args Nregs Code] | Y] Out ->
  (let . (pr (make-string "  (~A ~R ~R ~R~%" Head Name Args Nregs) Out)
       . (for-each (pretty-print-label-code Out) Code)
       . (pr (make-string "  )~%") Out)
    (pretty-print-code Y Out))
  where (element? Head [klvm-func klvm-toplevel klvm-closure])
  [X | Y] Out -> (let S (value *maximum-print-sequence-size*)
                      . (set *maximum-print-sequence-size* -1)
                      T1 (pr (make-string "  ~S~%" X) Out)
                      . (set *maximum-print-sequence-size* S)
                     (pretty-print-code Y Out)))

(define show-code
  X -> (pretty-print-code X (stoutput)))
)
