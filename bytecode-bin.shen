(package klvm.bytecode.bin [klvm.bytecode.walk klvm.bytecode.mk-backend
                            klvm.bytecode-from-kl

                            binary.mkbuf
                            binary.buf-size
                            binary.put-buf
                            binary.buf-buf

                            klvm.lambda
                            klvm.reg]

(set opcode-count 128)

(define opcode
  -> (let X (value opcode-count)
          . (set opcode-count (+ X 1))
        X))

(set op.load-reg-> (opcode))
(set op.load-fn-> (opcode))
(set op.load-const-> (opcode))
(set op.jump (opcode))
(set op.closure-lambda-> (opcode))
(set op.closure-reg-> (opcode))
(set op.closure-fn-> (opcode))
(set op.closure-tail-lambda-> (opcode))
(set op.closure-tail-reg-> (opcode))
(set op.closure-tail-fn-> (opcode))
(set op.drop-ret (opcode))
(set op.load-ret-> (opcode))
(set op.call (opcode))
(set op.tail-call (opcode))
(set op.jump-unless (opcode))
(set op.ret-reg (opcode))
(set op.ret-fn (opcode))
(set op.ret-const (opcode))
(set op.push-error-handler (opcode))
(set op.pop-error-handler (opcode))

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

(define const
  X C -> X)

(define loadreg
  To From _ Buf -> (do (binary.put-u8 (value op.load-reg->) Buf)
                       (binary.put-uint To Buf)
                       (binary.put-uint From Buf)))

(set backend (klvm.bytecode.mk-backend native mk-code code-len code-append!
                                       prep-code const loadreg loadfn
                                       loadconst jump closure-> closure-tail->
                                       funcall tailcall if-reg-expr retreg
                                       retfn retconst push-error-handler
                                       pop-error-handler emit-func))

(define walk
  X S+ -> (klvm.bytecode.walk X S+ (value backend)))

(define klvm.bytecode-from-kl
  X S+ -> (walk X S+)))
