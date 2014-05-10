(package klvm.s2 [denest.walk klvm-dump klvm.s1.func-next-reg

                  regkl.walk regkl.get-arg regkl.get-reg regkl.set-reg!
                  regkl.closure regkl.func regkl.toplevel regkl.freeze

                  klvm.native klvm.reg klvm.reg-> klvm.if klvm.tailif
                  klvm.call klvm.tailcall klvm.sp+ klvm.sp- klvm.nargs->
                  klvm.closure-> klvm.closure-nargs klvm.put-closure-args
                  klvm.closure-func klvm.nargs+ klvm.entry klvm.return
                  klvm.nargs-cond klvm.wipe-stack klvm.nargs klvm.func-obj
                  klvm.nargs+ klvm.nargs- klvm.next klvm.next-> klvm.labels
                  klvm.ret

                  klvm.s1.func klvm.s1.closure klvm.s1.toplevel
                  klvm.s1.return]
[])

(defstruct context
  (func-name symbol)
  (stack-size number)
  (nargs number)
  (nregs number)
  (label number)
  (func s-expr)
  (toplevel s-expr)
  (native (A --> context --> A)))

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

(define entry-template
  Name Nargs -> [klvm.nargs-cond
                 [[klvm.nregs-> [1]]
                  [klvm.ret-> [klvm.func-obj Name Nargs]]
                  [klvm.wipe-stack]
                  [klvm.return [klvm.next]]]
                 [[klvm.nargs- Nargs]]
                 [[klvm.sp+ [klvm.nargs]]
                  [klvm.sp- Nargs]
                  [klvm.nargs- Nargs]]])

(define return-template
  X Next -> [klvm.if-nargs>0
             [[klvm.closure-> X]
              [klvm.nregs-> [[klvm.nargs] [klvm.closure-nargs]]]
              [klvm.next-> Next]
              [klvm.wipe-stack]
              [klvm.put-closure-args 0]
              [klvm.sp- [klvm.nargs]]
              [klvm.call [klvm.closure-func]]]
             [[klvm.ret-> X]
              [klvm.next-> Next]
              [klvm.wipe-stack]
              [klvm.return [klvm.next]]]])

(define call-template
  Op F Nargs S -> [[klvm.sp+ S]
                   [klvm.closure-> F]
                   [klvm.put-closure-args Nargs]
                   [klvm.nargs-> Nargs]
                   [klvm.nargs+ [klvm.closure-nargs]]
                   [Op [klvm.closure-func]]])

(define walk-call
  F Nargs Ret-reg C Acc -> (let S (context-stack-size C)
                                Next (next-label C)
                                X [[klvm.next-> Next] | Acc]
                                X (prepend (call-template klvm.call F Nargs S)
                                           X)
                                X (label Next C X)
                                X [[klvm.wipe-stack] | X]
                                X [[klvm.sp- S] | X]
                                X [[klvm.reg-> Ret-reg [klvm.ret]] | X]
                             X))

(define walk-tailcall
  F Nargs C Acc -> (let S (context-stack-size C)
                        Acc [[klvm.next->
                              [klvm.reg (klvm.s1.func-next-reg C)]]
                             | Acc]
                     (prepend (call-template klvm.tailcall F Nargs S) Acc)))

(define walk-if-expr
  X X-label _ true C Acc -> (let L (label X-label C Acc)
                              (walk-x1 X C L))
  X X-label After-label false C Acc -> (let L (label X-label C Acc)
                                            Acc (walk-x1 X C L)
                                         [[klvm.goto After-label] | Acc]))

(define walk-if
  [klvm.reg R] Then Else Tail? C Acc -> 
  (let If-label (next-label C)
       After-label (next-label C)
       Acc [[klvm.goto If-label] | Acc]
       Then-label (next-label C)
       Acc (walk-if-expr Then Then-label After-label Tail? C Acc)
       Else-label (next-label C)
       Acc (walk-if-expr Else Else-label After-label Tail? C Acc)
       Acc (label If-label C Acc)
       X [klvm.if [klvm.reg R] [klvm.goto Then-label] [klvm.goto Else-label]]
       Acc [X | Acc]
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
  [klvm.call F Nargs Ret-reg] C Acc -> (walk-call F Nargs Ret-reg C Acc)
  [klvm.tailcall F Nargs] C Acc -> (walk-tailcall F Nargs C Acc)
  [klvm.reg-> R X] C Acc -> [[klvm.reg-> R X] | Acc]
  [klvm.return X Next] C Acc -> [[klvm.return X Next] | Acc])

(define func-hdr
  klvm.s1.func -> klvm.func
  klvm.s1.toplevel -> klvm.toplevel
  klvm.s1.closure -> klvm.closure)

(define func-entry
  C -> (let N (context-stack-size C)
            Acc (label (next-label C) C [])
         (prepend [[klvm.entry (context-func-name C) (context-nargs C)]
                   [klvm.nregs-> N]
                   [klvm.reg-> (- N 2) [klvm.nargs]]
                   [klvm.reg-> (- N 1) [klvm.next]]]
                  Acc)))

(define walk-toplevel-expr
  [Type Name Args Nregs Stack-size Code] Acc ->
  (let Nargs (length Args)
       C (mk-context Name Stack-size Nargs Nregs -1 [] Acc [])
       X (func-entry C)
       X (walk-x1 Code C X)
       X (close-label C X)
       Acc' (context-toplevel C)
       Hdr (func-hdr Type)
       Labels [klvm.labels | (reverse (context-func C))]
    [[Hdr Name Args Stack-size Labels] | Acc'])
  where (element? Type [klvm.s1.func klvm.s1.closure klvm.s1.toplevel]))

(define walk-toplevel
  [] Acc -> (reverse Acc)
  [X | Y] Acc -> (walk-toplevel Y (walk-toplevel-expr X Acc)))

(define klvm.s2.walk
  X -> (walk-toplevel X []))
