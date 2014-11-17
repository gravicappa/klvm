(package klvm.asm [denest.walk
                   regkl.trap-error klvm.asm-from-kl
                   
                   klvm.native klvm.reg klvm.reg-> klvm.call klvm.tailcall
                   klvm.tailif klvm.if klvm.return klvm.mk-closure
                   klvm.push-error-handler klvm.pop-error-handler klvm.lambda]
         _)

(defstruct context
  (func symbol)
  (type symbol)
  (frame-size number)
  (frame-size-extra number)
  (nargs number)
  (toplevel (list A))
  (jumps table))

(define mk-code
  -> [])

(define code-len
  X -> (length X))

(define code-append
  X Y -> (append Y X))

(define const
  X _ -> X)

(define loadreg
  To From _ Acc -> [[klvm.load-reg-> To From] | Acc])

(define loadfn
  To From _ Acc -> [[klvm.load-fn-> To From] | Acc])

(define loadconst
  To X C Acc -> [[klvm.load-const-> To (const X C)] | Acc])

(define jump
  Where _ Acc -> [[klvm.jump Where] | Acc])

(define closure->
  [klvm.lambda L] Nargs _ Acc -> [[klvm.closure-lambda-> L Nargs] | Acc]
  [klvm.reg R] Nargs _ Acc -> [[klvm.closure-reg-> R Nargs] | Acc]
  X Nargs _ Acc -> [[klvm.closure-fn-> X Nargs] | Acc] where (symbol? X))

(define closure-tail->
  [klvm.lambda L] Nargs _ Acc -> [[klvm.closure-tail-lambda-> L Nargs] | Acc]
  [klvm.reg R] Nargs _ Acc -> [[klvm.closure-tail-reg-> R Nargs] | Acc]
  X Nargs _ Acc -> [[klvm.closure-tail-fn-> X Nargs] | Acc] where (symbol? X))

(define funcall
  _ Acc -> [[klvm.call] | Acc])

(define tailcall
  _ Acc -> [[klvm.tail-call] | Acc])

(define if-reg-expr
  Reg Else-Offset _ Acc -> [[klvm.jmp-unless Reg Else-Offset] | Acc])

(define retreg
  Reg C Acc -> [[klvm.ret-reg Reg] | Acc])

(define retfn
  Fn C Acc -> [[klvm.ret-fn Fn] | Acc])

(define retconst
  X C Acc -> [[klvm.ret-const (const X C)] | Acc])

(define push-error-handler
  [klvm.reg Reg] C Acc -> [[klvm.push-error-handler Reg] | Acc])

(define pop-error-handler
  C Acc -> [[klvm.pop-error-handler] | Acc])

(define reg->
  To [klvm.reg From] C Acc -> (loadreg To From C Acc)
  To [klvm.lambda From] C Acc -> (loadfn To From C Acc)
  To X C Acc -> (loadconst To X C Acc) where (klvm.s1.const? X))

(define call'
  [] C Acc -> [[klvm.drop-ret] | (funcall C Acc)]
  Ret-reg C Acc -> [[klvm.load-ret-> Ret-reg] | (funcall C Acc)])

(define prepare-args
  [] _ _ Acc -> Acc
  [[I | X] | Xs] C Off Acc -> (let Acc (reg-> (+ I Off) X C Acc)
                                (prepare-args Xs C Off Acc)))

(define walk-call
  F Nargs Ret-reg X C Acc -> (let Acc (closure-> F Nargs C Acc)
                                  Off (context-frame-size C)
                                  Acc (prepare-args X C Off Acc)
                               (call' Ret-reg C Acc)))

(define walk-tailcall
  F Nargs X C Acc -> (let Acc (closure-tail-> F Nargs C Acc)
                          Acc (prepare-args X C 0 Acc)
                       (tailcall C Acc)))

(define walk-return
  [klvm.reg Reg] C Acc -> (retreg Reg C Acc)
  [klvm.lambda X] C Acc -> (retfn X C Acc)
  X C Acc -> (retconst X C Acc) where (klvm.s1.const? X))

(define if-jump
  Where true C Acc -> Acc
  Where false C Acc -> (jump Where C Acc))

(define then-code-len
  Code true -> (code-len Code)
  Code false -> (+ (code-len Code) 1))

(define walk-if
  [klvm.reg R] Then Else Tail? C Acc ->
  (let Then-code (walk-x1 Then C (mk-code))
       Else-code (walk-x1 Else C (mk-code))
       Then-code-len (then-code-len Then-code Tail?)
       Acc (if-reg-expr R Then-code-len C Acc)
       Acc (code-append Acc Then-code)
       Acc (if-jump (code-len Else-code) Tail? C Acc)
     (code-append Acc Else-code)))

(define walk-do
  [X] C Acc -> (walk-x1 X C Acc)
  [X | Xs] C Acc -> (walk-do Xs C (walk-x1 X C Acc)))

(define walk-x1
  [do | X] C Acc -> (walk-do X C Acc)
  [klvm.tailif If Then Else] C Acc -> (walk-if If Then Else true C Acc)
  [klvm.if If Then Else] C Acc -> (walk-if If Then Else false C Acc)
  [klvm.call F Nargs Ret-reg X] C Acc -> (walk-call F Nargs Ret-reg X C Acc)
  [klvm.tailcall F Nargs X] C Acc -> (walk-tailcall F Nargs X C Acc)
  [klvm.reg-> R X] C Acc -> (reg-> R X C Acc)
  [klvm.return X] C Acc -> (walk-return X C Acc)
  [klvm.push-error-handler E] C Acc -> (push-error-handler E C Acc)
  [klvm.pop-error-handler] C Acc -> (pop-error-handler C Acc)
  X _ _ -> (error "Unexpected L1 expression: ~S~%" X))

(define walk-toplevel-expr
  [Type Name Args Frame-size Frame-size-extra Code] S+ Acc ->
  (let Arity (length Args)
       Frame-size' (+ Frame-size S+)
       C (mk-context Name Type Frame-size' Frame-size-extra Arity Acc [])
       X (walk-x1 Code C [])
       Acc' (context-toplevel C)
    [[Type Name Args Frame-size' Frame-size-extra (reverse X)] | Acc']))

(define walk-toplevel
  [] S+ Acc -> (reverse Acc)
  [X | Xs] S+ Acc -> (walk-toplevel Xs S+ (walk-toplevel-expr X S+ Acc)))

(define klvm.asm.walk
  X S+ -> (walk-toplevel (klvm.s1.walk [] X) S+ []))
