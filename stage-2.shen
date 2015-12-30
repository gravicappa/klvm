(package klvm.s2 [regkl.trap-error
                  
                  klvm.call klvm.closure-> klvm.entry klvm.func klvm.func-obj
                  klvm.goto klvm.goto-next klvm.if klvm.if-nargs>0 klvm.nargs
                  klvm.nargs- klvm.nargs-> klvm.nargs+ klvm.nargs-cond
                  klvm.next klvm.next-> klvm.nregs-> klvm.pop-error-handler
                  klvm.push-error-handler klvm.put-closure-args klvm.reg
                  klvm.reg-> klvm.ret klvm.ret-> klvm.return klvm.runtime
                  klvm.sp+ klvm.sp- klvm.tailcall klvm.tailif klvm.thaw
                  klvm.toplevel klvm.wipe klvm.lambda

                  klvm.entry-template klvm.return-template

                  klvm.s1.func klvm.s1.lambda klvm.s1.toplevel klvm.s1.return
                  klvm.s1.translate]

(defstruct context
  (func-name symbol)
  (func-type symbol)
  (frame-size number)
  (frame-size-extra number)
  (arity number)
  (label number)
  (func s-expr)
  (toplevel s-expr))

(define next-reg C -> (- (context-frame-size C) 1))
(define nargs-reg C -> (- (context-frame-size C) 2))

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
                 [N]))

(define klvm.entry-template
  Func Arity Name -> [klvm.nargs-cond
                      Arity
                      [[klvm.ret-> [klvm.func-obj Func Arity Name]]
                       [klvm.wipe 0]
                       [klvm.goto-next]]
                      [[klvm.nargs- Arity]]
                      [[klvm.sp+ [klvm.nargs]]
                       [klvm.sp- Arity]
                       [klvm.nargs- Arity]]])

(define klvm.return-template
  X Next -> [klvm.if-nargs>0
             [[klvm.closure-> X]
              [klvm.next-> Next]
              [klvm.sp- [klvm.nargs]]
              [klvm.put-closure-args]
              [klvm.wipe [klvm.nargs]]
              [klvm.call]]
             [[klvm.ret-> X]
              [klvm.next-> Next]
              [klvm.wipe 0]
              [klvm.goto-next]]])

(define call-template
  [klvm.lambda _] Nargs S+ -> [[klvm.sp+ S+]
                               [klvm.nargs-> Nargs]
                               [klvm.call lambda]]
  _ Nargs S+ -> [[klvm.sp+ S+]
                 [klvm.nargs-> Nargs]
                 [klvm.put-closure-args]
                 [klvm.call]])

(define tailcall-template
  [klvm.lambda _] Nargs Framesize C -> [[klvm.sp- [klvm.nargs]]
                                        [klvm.nargs+ Nargs]
                                        [klvm.wipe [klvm.nargs]]
                                        [klvm.call lambda]]
  _ Nargs Framesize C -> [[klvm.sp- [klvm.nargs]]
                          [klvm.nargs+ Nargs]
                          [klvm.put-closure-args]
                          [klvm.wipe [klvm.nargs]]
                          [klvm.call]])

(define entry-func-name
  Name klvm.s1.func -> Name
  _ _ -> [])

(define entry-op
  Name Arity Type -> [klvm.entry Name Arity (entry-func-name Name Type)])

(define return-op
  X C -> [klvm.return X (next-reg C)])

(define call-ret
  [] Acc -> Acc
  Ret-reg Acc -> [[klvm.reg-> Ret-reg [klvm.ret]] | Acc])

(define prepare-args
  [] Off _ Acc -> Acc
  [[I | X] | Xs] Off C Acc -> (let Acc [[klvm.reg-> (+ Off I) X] | Acc]
                                (prepare-args Xs Off C Acc)))

(define walk-call
  F Nargs Ret-reg Prep C Acc -> (let S (context-frame-size C)
                                     Next (next-label C)
                                     X [[klvm.next-> Next] | Acc]
                                     X [[klvm.closure-> F] | X]
                                     X (prepare-args Prep S C X)
                                     X (prepend (call-template F Nargs S) X)
                                     X (label Next C X)
                                     X [[klvm.sp- S] | X]
                                     X (call-ret Ret-reg X)
                                  X))

(define walk-tailcall
  F Nargs Prep C Acc -> (let Acc [[klvm.next-> [klvm.reg (next-reg C)]] | Acc]
                             Acc [[klvm.closure-> F] | Acc]
                             Acc [[klvm.nargs-> [klvm.reg (nargs-reg C)]]
                                  | Acc]
                             Acc (prepare-args Prep 0 C Acc)
                             S (+ (context-frame-size C)
                                  (context-frame-size-extra C))
                          (prepend (tailcall-template F Nargs S C) Acc)))

(define walk-if-expr
  X X-label _ true C Acc -> (walk-x1 X C (label X-label C Acc))
  X X-label After-label false C Acc -> (let L (label X-label C Acc)
                                            Acc (walk-x1 X C L)
                                         [[klvm.goto After-label] | Acc]))

(define walk-if
  [klvm.reg R] Then Else Tail? C Acc -> 
  (let After-label (next-label C)
       Then-label (next-label C)
       Else-label (next-label C)
       X [klvm.if [klvm.reg R] [klvm.goto Then-label] [klvm.goto Else-label]]
       Acc [X | Acc]
       Acc (walk-if-expr Then Then-label After-label Tail? C Acc)
       Acc (walk-if-expr Else Else-label After-label Tail? C Acc)
    (if Tail?
        Acc
        (label After-label C Acc))))

(define walk-do
  [X] C Acc -> (walk-x1 X C Acc)
  [X | Xs] C Acc -> (walk-do Xs C (walk-x1 X C Acc)))

(define walk-x1
  [do | Xs] C Acc -> (walk-do Xs C Acc)
  [klvm.tailif If Then Else] C Acc -> (walk-if If Then Else true C Acc)
  [klvm.if If Then Else] C Acc -> (walk-if If Then Else false C Acc)
  [klvm.call F Nargs Ret-reg X] C Acc -> (walk-call F Nargs Ret-reg X C Acc)
  [klvm.tailcall F Nargs X] C Acc -> (walk-tailcall F Nargs X C Acc)
  [klvm.reg-> R X] C Acc -> [[klvm.reg-> R X] | Acc]
  [klvm.return X] C Acc -> (let R (nargs-reg C)
                                Acc' [[klvm.nargs-> [klvm.reg R]] | Acc]
                             [(return-op X C) | Acc'])
  [klvm.push-error-handler E] C Acc -> [[klvm.push-error-handler E] | Acc]
  [klvm.pop-error-handler] C Acc -> [[klvm.pop-error-handler] | Acc]
  [X | Xs] _ Acc -> [[X | Xs] | Acc]
  _ _ Acc -> Acc)

(define func-hdr
  klvm.s1.func -> klvm.func
  klvm.s1.toplevel -> klvm.toplevel
  klvm.s1.lambda -> klvm.lambda)

(define func-entry
  C -> (prepend [(entry-op (context-func-name C) (context-arity C)
                           (context-func-type C))
                 [klvm.nregs-> (+ (context-frame-size C)
                                  (context-frame-size-extra C))]
                 [klvm.reg-> (nargs-reg C) [klvm.nargs]]
                 [klvm.reg-> (next-reg C) [klvm.next]]]
                (label (next-label C) C [])))

(define walk-toplevel-expr
  [Type Name Args Frame-size Frame-size-extra Code] Acc ->
  (let Arity (length Args)
       Frame-size' (+ Frame-size 2)
       C (mk-context Name Type Frame-size' Frame-size-extra Arity -1 [] Acc)
       X (func-entry C)
       X (walk-x1 Code C X)
       X (close-label C X)
       Acc' (context-toplevel C)
       Hdr (func-hdr Type)
       Labels (reverse (context-func C))
    [[Hdr Name Args Frame-size' Labels] | Acc'])
  where (element? Type [klvm.s1.func klvm.s1.lambda klvm.s1.toplevel]))

(define walk-toplevel
  [] Acc -> (reverse Acc)
  [X | Y] Acc -> (walk-toplevel Y (walk-toplevel-expr X Acc)))

(define translate
  X -> (walk-toplevel X []))

(define klvm.runtime
  -> (let X (intern "X")
          E (intern "E")
          R (intern "R")
      [[defun regkl.trap-error [X E]
         [do [klvm.push-error-handler E]
             [let R [X]
               [do [klvm.pop-error-handler]
                   R]]]]]))

(define klvm.s2.from-kl
  Fn Kl Elim-toplevel-atoms? ->
  (translate (klvm.s1.translate Fn Kl Elim-toplevel-atoms?)))
)
