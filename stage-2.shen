(package klvm.s2 [denest.walk klvm-dump
                  regkl.walk regkl.get-arg regkl.get-reg regkl.set-reg!
                  regkl.closure regkl.func regkl.toplevel regkl.freeze
                  klvm.native klvm.reg klvm.reg->
                  klvm.s1.func klvm.s1.closure klvm.s1.toplevel]
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

(define walk-call
  F Nargs Ret-reg C Acc -> )

(define walk-tailcall
  F Nargs C Acc -> )

(define emit-if-expr
  X X-label _ true C Acc -> (let L (label X-label C Acc)
                              (walk-x1 X C L))
  X X-label After-label false C Acc -> (let L (label X-label C Acc)
                                            Acc (walk-x1 X C L)
                                         [[klvm-goto After-label] | Acc]))

(define walk-if
  If [klvm.reg R] Else Tail? C Acc -> 
  (let If-label (next-label C)
       After-label (next-label C)
       Acc [[klvm.goto If-label] | Acc]
       Then-label (next-label C)
       Acc (walk-if-expr Then Then-label After-label Tail? C Acc)
       Else-label (next-label C)
       Acc (walk-if-expr Elst Else-label After-label Tail? C Acc)
       Acc (label If-label C Acc)
       X [klvm.if [klvm.reg R] [klvm-goto Then-label] [klvm-goto Else-label]]
       Acc [X | Acc])
    (if Tail?
        Acc
        (label After-label C Acc)))

(define walk-do
  [X] C Acc -> (walk-x1 X C Acc)
  [X | Xs] C Acc -> (walk-do Xs C (walk-x1 X C Acc)))

(define walk-x1
  [do | Xs] C Acc -> (walk-do Xs C Acc)
  [klvm.tailif If Then Else] C Acc -> (walk-if If Then Else true C Acc)
  [klvm.if If Then Else] C Acc -> (walk-if If Then Else false C Acc)
  [klvm.call F Nargs Ret-reg] C Acc -> (walk-call F Nargs Ret-reg C Acc)
  [klvm.tailcall F Nargs] C Acc -> (walk-tailcall F Nargs C Acc)
  )

(define func-hdr
  klvm.s1.func -> klvm.func
  klvm.s1.toplevel -> klvm.toplevel
  klvm.s1.closure -> klvm.closure)

(define walk-toplevel-expr
  [Type Name Args Nregs Stack-size Code] ->
  (let Nargs (length Args)
       C (mk-context Name Stack-size Nargs Nregs -1 [] [] [])
       X (func-entry C)
       X (walk-x1 Code C X)
       X (close-label C X)
       Acc (context-toplevel C)
       Hdr (func-hdr Type)
    [[Hdr Name Args Stack-size (reverse (context-func C))] | Acc])
  where (element? Type [klvm.func klvm.closure klvm.toplevel]))

(define walk-toplevel
  [] Acc -> (reverse Acc)
  [X | Y] Acc -> (walk-toplevel Y (walk-toplevel-expr X Acc)))

(define klvm.s2.walk
  X -> (walk-toplevel X []))
