(package klvm.bytecode.bin [klvm.bytecode.walk
                            klvm.bytecode.mk-backend'
                            klvm.bytecode.def-enum
                            klvm.bytecode-from-kl

                            binary.mkbuf
                            binary.buf-size
                            binary.uint-size
                            binary.sint-size
                            binary.put-buf
                            binary.buf-buf
                            binary.put-u8
                            binary.put-u16
                            binary.put-u32
                            binary.put-i8
                            binary.put-i16
                            binary.put-i32

                            klvm.lambda
                            klvm.reg]

(set magic 1424424960) \\ #x54e70000

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
  op.+u8
  op.+u16
  op.+u32
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
  op.push-error-handler
  op.pop-error-handler
  op.ret-reg
  op.ret-const
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

(define const-lambda
  L C -> (klvm.bytecode.const* L lambda C))

(define const-func
  F C -> (klvm.bytecode.const* F func C))

(define native
  _ _ -> (fail))

(define mk-code
  -> (binary.mkbuf 16))

(define code-len
  Buf -> (binary.buf-size Buf))

(define code-append!
  To From -> (binary.put-buf From To))

(define op-value+
  0 Buf -> Buf
  X Buf -> (binary.put-u8 X (binary.put-u8 (op.+u8) Buf))
           where (<= (binary.uint-size X) 1)
  X Buf -> (binary.put-u16 X (binary.put-u8 (op.+u16) Buf))
           where (<= (binary.uint-size X) 2)
  X Buf -> (binary.put-u32 X (binary.put-u8 (op.+u32) Buf))
           where (<= (binary.uint-size X) 4)
  X _ -> (error "~A: index value is too large ~A" op-value+ X))

(define op-value
  X _ _ -> (error "~A: Negative argument: ~A" op-value X) where (< X 0)
  X Mask Buf -> (binary.put-u8 (xbin (| X Mask)) Buf) where (<= X 63)
  X Mask Buf -> (let X' (binary.arithmetic-shift X -5)
                  (op-value X Mask (op-value+ X' Buf))))

(define op-value/1
  X Buf -> (op-value X 0 Buf))

(define op-value/2
  X Buf -> (op-value X (bin 01000000) Buf))

(define reg->
  X Buf -> (op-value/1 X Buf))

(define ret->
  X Buf -> (op-value/2 X Buf))

(define <-reg
  X Buf -> (op-value/1 X Buf))

(define <-const
  X Buf -> (op-value/2 X Buf))

(define op1
  Op X Buf -> (binary.put-u8 Op (op-value/1 X Buf)))

(define op2
  Op X Y Buf -> (let Buf (op-value/1 X Buf)
                     Buf (binary.put-u8 Op Buf)
                  (op-value/1 Y Buf)))

(define ret-reg
  Reg _ Buf -> (op1 (op.ret-reg) Reg Buf))

(define ret-lambda
  Fn C Buf -> (op1 (op.ret-const) (const Fn func C) Buf))

(define ret-const
  X C Buf -> (op1 (op.ret-const) (const X C) Buf))

(define load-reg
  To From _ Buf -> (<-reg From (reg-> To Buf)))

(define load-lambda
  To From C Buf -> (<-const (const-lambda From C) (reg-> To Buf)))

(define load-const
  To X C Buf -> (<-const (const X C) (reg-> To Buf)))

(define jump
  Where C Buf -> (op1 (op.jump) (const Where C) Buf))

(define closure->
  [klvm.lambda L] Nargs C Buf -> (let X (const-lambda L C)
                                   (op2 (op.closure-lambda->) X Nargs Buf))
  [klvm.reg R] Nargs C Buf -> (op2 (op.closure-reg->) R Nargs Buf)
  X Nargs C Buf -> (op2 (op.closure-fn->) (const-func X C) Nargs Buf))

(define closure-tail->
  [klvm.lambda L] Nargs C Buf -> (let Op (op.closure-tail-lambda->)
                                   (op2 Op (const-lambda L C) Nargs Buf))
  [klvm.reg R] Nargs C Buf -> (op2 (op.closure-tail-reg->) R Nargs Buf)
  X Nargs C Buf -> (op2 (op.closure-tail-fn->) (const-func X C) Nargs Buf))

(define call-arg
  [klvm.reg From] _ Buf -> (op-value/1 From Buf)
  [klvm.lambda L] C Buf -> (op-value/1 (const-lambda L C) Buf)
  X C Buf -> (op-value/2 (const X C) Buf))

(define put-call-args
  [] _ C Buf -> Buf
  [[I | X] | Xs] I C Buf -> (put-call-args Xs (+ I 1) C (call-arg X C Buf))
  [[I | X] | Xs] _ C Buf -> (error "~A: Non tailcall args are not sequential."
                                   put-call-args))

(define funcall-end
  [] Buf -> (binary.put-u8 (op.drop-ret) Buf)
  Ret-reg Buf -> (ret-> Ret-reg Buf))

(define funcall
  F Nargs Ret-reg Args C Buf -> (let Buf (closure-> F Nargs C Buf)
                                     Buf (put-call-args Args 0 C Buf)
                                  (funcall-end Ret-reg Buf)))

(define put-tailcall-args
  [] C Buf -> Buf
  [[I | X] | Xs] C Buf -> (let Buf (call-arg X C Buf)
                            (put-tailcall-args Xs C (reg-> I Buf))))

(define tailcall
  F Nargs Args C Buf -> (let Buf (closure-tail-> F Nargs C Buf)
                             Buf (put-tailcall-args Args C Buf)
                          (binary.put-u8 (op.tail-call))))

(define if-reg-expr
  Reg Else-Offset C Buf -> (op2 (op.jump-unless) Reg Else-Offset Buf))

(define push-error-handler
  [klvm.reg Reg] _ Buf -> (op1 (op.push-error-handler) Reg Buf))

(define pop-error-handler
  _ Buf -> (binary.put-u8 (op.pop-error-handler) Buf))

\*
(define emit-func
  Type Name Args Frame-size Frame-size-extra Code C Buf ->
  (let Const (reverse (klvm.bytecode.context-const C))
       Code' (reverse Code)
    [[Type Name Args Frame-size Frame-size-extra Const Code'] | Acc]))
*\
(define emit-func
  Type Name Args Frame-size Frame-size-extra Code C Buf -> _)

(define put-const
  C Buf -> Buf)

(define put-toplevel
  C Buf -> Buf)

(define module-header
  C -> (let Buf (binary.mkbuf 16)
            Buf (binary.put-u32 (magic))
            Buf (put-const C Buf)
            Buf (put-toplevel C Buf)
          Buf))

(define prep-code
  C Buf -> (let Header (module-header C)
                Buf (binary.put-buf Buf Header)
             (binary.buf-buf Buf)))

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

(define from-kl
  X S+ -> (klvm.bytecode.compile X S+ (value backend))))
