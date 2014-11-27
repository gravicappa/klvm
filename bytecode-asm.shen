(package klvm.bytecode.asm [klvm.bytecode.walk klvm.bytecode.mk-backend
                            klvm.bytecode-asm-from-kl

                            klvm.lambda
                            klvm.reg
                            
                            klvm.load-reg->
                            klvm.load-fn->
                            klvm.load-const->
                            klvm.jump
                            klvm.closure-lambda->
                            klvm.closure-reg->
                            klvm.closure-fn->
                            klvm.closure-tail-lambda->
                            klvm.closure-tail-reg->
                            klvm.closure-tail-fn->
                            klvm.drop-ret
                            klvm.load-ret->
                            klvm.call
                            klvm.tail-call
                            klvm.jump-unless
                            klvm.ret-reg
                            klvm.ret-fn
                            klvm.ret-const
                            klvm.push-error-handler
                            klvm.pop-error-handler
                            ]

(define mk-code
  -> [])

(define code-len
  X -> (length X))

(define code-append
  X Y -> (append Y X))

(define prep-code
  X -> (reverse X))

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
  [] _ Acc -> [[klvm.drop-ret] [klvm.call] | Acc]
  Ret-reg _ Acc -> [[klvm.load-ret-> Ret-reg] [klvm.call] | Acc])

(define tailcall
  _ Acc -> [[klvm.tail-call] | Acc])

(define if-reg-expr
  Reg Else-Offset _ Acc -> [[klvm.jump-unless Reg Else-Offset] | Acc])

(define retreg
  Reg C Acc -> [[klvm.ret-reg Reg] | Acc])

(define retfn
  Fn C Acc -> [[klvm.ret-fn Fn] | Acc])

(define retconst
  X C Acc -> [[klvm.ret-const (const X C)] | Acc])

(define push-error-handler
  [klvm.reg Reg] _ Acc -> [[klvm.push-error-handler Reg] | Acc])

(define pop-error-handler
  _ Acc -> [[klvm.pop-error-handler] | Acc])

(define emit-func
  Type Name Args Frame-size Frame-size-extra Code _ Acc ->
  [[Type Name Args Frame-size Frame-size-extra (reverse Code)] | Acc])

(set backend (klvm.bytecode.mk-backend [] mk-code code-len code-append
                                       prep-code const loadreg loadfn
                                       loadconst jump closure-> closure-tail->
                                       funcall tailcall if-reg-expr retreg
                                       retfn retconst push-error-handler
                                       pop-error-handler emit-func))

(define walk
  X S+ -> (klvm.bytecode.walk X S+ (value backend)))

(define klvm.bytecode-asm-from-kl
  X S+ -> (walk X S+))
)
