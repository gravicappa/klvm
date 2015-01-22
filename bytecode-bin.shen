(package klvm.bytecode.bin [klvm.bytecode.walk klvm.bytecode.mk-backend
                            klvm.bytecode.def-enum
                            klvm.bytecode-from-kl

                            binary.mkbuf
                            binary.buf-size
                            binary.put-buf
                            binary.buf-buf

                            klvm.lambda
                            klvm.reg]

(klvm.bytecode.def-enum
  const.type.nil
  const.type.s16
  const.type.s32
  const.type.func
  const.type.str
  const.type.sym
  const.type.vec)

(klvm.bytecode.def-enum
  op.load-reg->
  op.load-fn->
  op.load-const->
  op.jump
  op.closure-lambda->
  op.closure-reg->
  op.closure-fn->
  op.closure-tail-lambda->
  op.closure-tail-reg->
  op.closure-tail-fn->
  op.drop-ret
  op.load-ret->
  op.call
  op.tail-call
  op.jump-unless
  op.ret-reg
  op.ret-fn
  op.ret-const
  op.push-error-handler
  op.pop-error-handler)

(define const-type
  [] -> nil
  X -> bool where (boolean? X)
  X -> str where (string? X)
  X -> sym where (symbol? X)
  X -> i16 where (and (number? X) (<= (binary.sint-size X) 2))
  X -> i32 where (and (number? X) (<= (binary.sint-size X) 4))
  X -> i64 where (and (number? X) (<= (binary.sint-size X) 8)))

(define native
  _ _ -> (fail))

(define mk-code
  -> (binary.mkbuf 16))

(define code-len
  Buf -> (binary.buf-size Buf))

(define code-append!
  To From -> (binary.put-buf From To))

(define prep-code
  Buf -> (binary.buf-buf Buf))

(define loadreg
  To From _ Buf -> (do (binary.put-u8 (op.load-reg->) Buf)
                       (binary.put-u8 To Buf)
                       (binary.put-u8 From Buf))
                   where (and (< To 256) (< From 256))
  To From _ Buf -> (error "klvm.binary.loadreg: register numbers > 256"))

(define loadfn*
  To From-idx C Buf -> (do (binary.put-u8 (op.load-fn->) Buf)
                           (binary.put-u8 To Buf)
                           (binary.put-u8 From-idx Buf))
                   where (and (< To 256) (< From-idx 256))
  To From _ Buf -> (error "klvm.binary.loadfn: register numbers > 256"))

(define loadfn
  To From C Buf -> (loadfn* To (klvm.bytecode.const* From fn C) C Buf))


(set backend (klvm.bytecode.mk-backend native mk-code code-len code-append!
                                       prep-code loadreg loadfn loadconst jump
                                       closure-> closure-tail-> funcall
                                       tailcall if-reg-expr retreg retfn
                                       retconst push-error-handler
                                       pop-error-handler emit-func))

(define walk
  X S+ -> (klvm.bytecode.walk X S+ (value backend)))

(define klvm.bytecode-from-kl
  X S+ -> (walk X S+)))
