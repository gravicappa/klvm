(package klvm.bytecode.bin [klvm.bytecode.walk
                            klvm.bytecode.mk-backend'
                            klvm.bytecode.def-enum
                            klvm.bytecode-from-kl

                            binary.mkbuf
                            binary.buf-size
                            binary.put-buf
                            binary.buf-buf

                            klvm.lambda
                            klvm.reg]

(klvm.bytecode.def-enum
  type.nil
  type.bool
  type.func
  type.lambda
  type.str
  type.sym
  type.vec
  type.i16
  type.i32
  type.i64)

(klvm.bytecode.def-enum
  (op.nop 128)
  op.a16
  op.a32
  op.b16
  op.b32
  op.load-reg->
  op.load-lambda->
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
  op.tail-call
  op.jump-unless
  op.ret-reg
  op.ret-lambda
  op.ret-const
  op.push-error-handler
  op.pop-error-handler
  op.+
  op.-
  op.*
  op./
  op.=
  op.>
  op.<
  op.>=
  op.<=
  op./=)

(define arg.const1 -> (bin 10000000))
(define arg.const2 -> (bin 11000000))
(define arg.const3 -> (bin 11100000))
(define arg.reg2 -> (bin 11111111))
(define arg.reg3 -> (bin 11111110))

(define const-type
  [] -> nil
  X -> bool where (boolean? X)
  X -> str where (string? X)
  X -> sym where (symbol? X)
  X -> i16 where (and (number? X) (<= (binary.sint-size X) 2))
  X -> i32 where (and (number? X) (<= (binary.sint-size X) 4))
  X -> i64 where (and (number? X) (<= (binary.sint-size X) 8)))

(define const-type-op
  nil -> (type.nil)
  bool -> (type.bool)
  func -> (type.func)
  lambda -> (type.lambda)
  str -> (type.str)
  sym -> (type.sym)
  vec -> (type.vec)
  i16 -> (type.i16)
  i32 -> (type.i32)
  i64 -> (type.i64))

(define const
  X C -> (klvm.bytecode.const X (const-type X) C))

(define native
  _ _ -> (fail))

(define mk-code
  -> (binary.mkbuf 16))

(define code-len
  Buf -> (binary.buf-size Buf))

(define code-append!
  To From -> (binary.put-buf From To))

(define arg1
  X _ -> (error "Negative argument: ~A" X) where (< X 0)
  X Buf -> (binary.put-u8 X Buf) where (< X 128)
  X Buf -> (do (binary.put-u8 (op.a16) Buf)
               (binary.put-u16 X Buf))
           where (<= (binary.uint-size X) 2)
  X Buf -> (do (binary.put-u8 (op.a32) Buf)
               (binary.put-u32 X Buf))
           where (<= (binary.uint-size X) 4)
  X _ -> (error "Argument is unexpectedly large: ~A" X))

(define op1
  Op X Buf -> (do (binary.put-u8 Op Buf)
                  (binary.put-u8 X Buf))
              where (= (binary.uint-size X) 1)
  Op X Buf -> (do (binary.put-u8 (op.b16) Buf)
                  (binary.put-u8 (binary.int#1 X) Buf)
                  (op2' Op (binary.int#2 X) Buf))
              where (= (binary.uint-size X) 2)
  Op X Buf -> (do (binary.put-u8 (op.b32) Buf)
                  (binary.put-u8 (binary.int#1 X) Buf)
                  (binary.put-u8 (binary.int#2 X) Buf)
                  (binary.put-u8 (binary.int#3 X) Buf)
                  (op2' Op (binary.int#4 X) Buf))
              where (= (binary.uint-size X) 4)
  _ X _ -> (error "Argument is unexpectedly large: ~A" X))

(define op2
  Op X Y C Buf -> (do (arg1 X Buf)
                      (op1 Op Y Buf)))

(define load-reg
  To From _ Buf -> (op2 (op.load-reg->) To From Buf))

(define load-lambda
  To From C Buf -> (let X (klvm.bytecode.const* From lambda C)
                     (op2 (op.load-lambda->) To X Buf)))

(define load-const
  To X C Buf -> (op2 (op.load-const->) To (const X C) Buf))

(define jump
  Where C Buf -> (op1 (op.jump) (const Where C) Buf))

(define closure->
  [klvm.lambda L] Nargs C Buf -> (let X (klvm.bytecode-const* L lambda C)
                                   (op2 (op.closure-lambda->) X Nargs Buf))
  [klvm.reg R] Nargs C Buf -> (op2 (op.closure-reg->) R Nargs Buf)
  X Nargs C Buf -> (let X' (klvm.bytecode-const* X func C)
                     (op2 (op.closure-fn->) X' Nargs Buf)))

(define closure-tail->
  [klvm.lambda L] Nargs C Buf -> (let X (klvm.bytecode-const* L lambda C)
                                   (op2 (op.closure-tail-lambda->) X Nargs
                                        Buf))
  [klvm.reg R] Nargs C Buf -> (op2 (op.closure-tail-reg->) R Nargs Buf)
  X Nargs C Buf -> (let X' (klvm.bytecode-const* X func C)
                     (op2 (op.closure-tail-fn->) X' Nargs Buf)))

(define funcall
  _ [] _ Buf -> (do (binary.put-u8 (op.call) Buf)
                    (binary.put-u8 (op.drop-ret) Buf))
  _ Ret-reg _ Buf -> (do (binary.put-u8 (op.call))
                         (op1 (op.load-ret->) Ret-reg Buf)))

(define encode-const-arg
  X Buf -> (binary.put-u8 (+ X (arg.const1)) Buf) where (<= X 63)

  X Buf -> (do (binary.put-u8 (+ (binary.bitwise-and X 31) (arg.const2)) Buf)
               (binary.put-u8 (binary.arithmetic-shift X -5) Buf))
           where (<= X (xbin (| (<< 255 5) 31)))

  X Buf -> (do (binary.put-u8 (+ (binary.bitwise-and X 15) (arg.const3)) Buf)
               (binary.put-u8 (binary.bitwise-ior
                               (binary.arithmetic-shift X -4) 255)
                              Buf)
               (binary.put-u8 (binary.bitwise-ior
                               (binary.arithmetic-shift X -12) 255)
                              Buf))
           where (<= X (xbin (| (<< 65535 12) (<< 255 4) 15)))

  X _ -> (error "Const argument is unexpectedly large: ~A" X))

(define call-arg
  [klvm.reg From] _ Buf -> (arg1 From Buf)
  [klvm.lambda L] C Buf -> (let X (klvm.bytecode-const* L lambda C)
                             (encode-const-arg X Buf))
  X C Buf -> (encode-const-arg (const X C) Buf))

(define put-call-args
  [] _ C Buf -> Buf
  [[I | X] | Xs] I C Buf -> (put-call-args Xs (+ I 1) C (call-arg X C Buf))
  [[I | X] | Xs] _ C Buf -> (error "Non tailcall args is not sequential."))

(define funcall
  F Nargs Ret-reg Args C Buf -> (let Buf (closure-> F Nargs C Buf)
                                  (call-args Args 0 C Buf)))

(define tailcall
  _ _ Buf -> (binary.put-u8 (op.tail-call)))

(define if-reg-expr
  Reg Else-Offset C Buf -> (let Off' (const Else-Offset C)
                             (op2 (op.jump-unless) Off' Reg Buf)))

(define ret-reg
  Reg _ Buf -> (op1 (op.ret-reg) Reg Buf))

(define ret-lambda
  Fn C Buf -> (op1 (op.ret-lambda) (const Fn func C) Buf))

(define ret-const
  X C Buf -> (op1 (op.ret-const) (const X C) Buf))

(define push-error-handler
  [klvm.reg Reg] _ Buf -> (op1 (op.push-error-handler) Reg Buf))

(define pop-error-handler
  _ Buf -> (binary.put-u8 (op.pop-error-handler) Buf))

(define emit-func
  Type Name Args Frame-size Frame-size-extra Code C Acc ->
  (let Const (reverse (klvm.bytecode.context-const C))
       Code' (reverse Code)
    [[Type Name Args Frame-size Frame-size-extra Const Code'] | Acc]))

(define prep-code
  C Buf -> (binary.buf-buf Buf))

(set backend (klvm.bytecode.mk-backend' mk-code
                                        code-len
                                        code-append!
                                        prep-code
                                        funcall
                                        tailcall
                                        load-reg
                                        load-lambda
                                        load-const
                                        jump
                                        if-reg-expr
                                        ret-reg
                                        ret-lambda
                                        ret-const
                                        push-error-handler
                                        pop-error-handler
                                        emit-func))

(define walk
  X S+ -> (klvm.bytecode.walk X S+ (value backend)))

(define klvm.bytecode-from-kl
  X S+ -> (walk X S+)))
