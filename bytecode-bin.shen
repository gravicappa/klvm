(package klvm.bytecode.bin [klvm.bytecode.walk
                            klvm.bytecode.compile
                            klvm.bytecode.mk-backend'
                            klvm.bytecode.def-enum
                            klvm.bytecode-from-kl
                            klvm.bytecode.context-local-const
                            klvm.bytecode.context-global-const
                            klvm.bytecode.context-frame-size
                            klvm.bytecode.const 

                            klvm.s1.func
                            klvm.s1.lambda
                            klvm.s1.toplevel

                            binary.mkstream
                            binary.bytestream-size
                            binary.uint8?
                            binary.uint16?
                            binary.uint32?
                            binary.uint64?
                            binary.int8?
                            binary.int16?
                            binary.int32?
                            binary.int64?
                            binary.sint-size
                            binary.put-bytestream
                            binary.put-u8
                            binary.put-u16
                            binary.put-u32
                            binary.put-i8
                            binary.put-i16
                            binary.put-i32
                            binary.put-string
                            binary.bitwise-ior
                            binary.arithmetic-shift
                            binary.bytevector-from-bytestream

                            klvm.lambda
                            klvm.reg]

\\ TODO: make hex reader macro
(define magic -> 1424424960) \\ #x54e70000

(klvm.bytecode.def-enum
  type.nil
  type.bool/true
  type.bool/false
  type.fail
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

(define const-type
  [] -> nil
  X -> bool where (= X true)
  X -> bool where (= X false)
  X -> fail where (= X (fail))
  X -> str where (string? X)
  X -> sym where (symbol? X)
  X -> i16 where (and (number? X) (binary.int16? X))
  X -> i32 where (and (number? X) (binary.int32? X))
  X -> i64 where (and (number? X) (binary.int64? X)))

(define const
  X C -> (klvm.bytecode.const X (const-type X) C))

(define const-lambda
  L C -> (klvm.bytecode.const L lambda C))

(define const-func
  F C -> (klvm.bytecode.const F func C))

(define native
  _ _ -> (fail))

(define mk-code
  -> (binary.mkstream))

(define code-len
  Buf -> (binary.bytestream-size Buf))

(define code-append!
  To From -> (binary.put-bytestream From To))

(define op-value+
  0 Buf -> Buf
  X Buf -> (binary.put-u8 X (binary.put-u8 (op.+u8) Buf))
           where (binary.uint8? X)
  X Buf -> (binary.put-u16 X (binary.put-u8 (op.+u16) Buf))
           where (binary.uint16? X)
  X Buf -> (binary.put-u32 X (binary.put-u8 (op.+u32) Buf))
           where (binary.uint32? X)
  X _ -> (error "~A: value is too large ~A" op-value+ X))

(define op-value
  X _ _ -> (error "~A: Negative argument: ~A" op-value X) where (< X 0)
  X Mask Buf -> (binary.put-u8 (binary.bitwise-ior X Mask) Buf)where (< X 64)
  X Mask Buf -> (let X' (binary.arithmetic-shift X -5)
                  (op-value X Mask (op-value+ X' Buf))))

(define const-ref
  X Buf -> (binary.put-u8 X Buf) where (< X 128)
  X Buf -> (binary.put-u8 X (binary.put-u8 (op.+u8) Buf))
           where (binary.uint8? X)
  X Buf -> (binary.put-u16 X (binary.put-u8 (op.+u16) Buf))
           where (binary.uint16? X)
  X Buf -> (binary.put-u32 X (binary.put-u8 (op.+u32) Buf))
           where (binary.uint32? X)
  X _ -> (error "~A: value is too large ~A" const-ref X))

(define op-value/1
  X Buf -> (op-value X 0 Buf))

(define op-value/2
  X Buf -> (op-value X (klvm.bytecode.bin 01000000) Buf))

(define strlen'
  S I -> (trap-error (do (pos S I)
                         (strlen' S (+ I 1)))
                     (/. E I)))

(define strlen
  S -> (strlen' S 0))

(define put-str
  S Buf -> (let Buf (binary.put-u32 (strlen S) Buf)
              (binary.put-string S Buf)))

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
  Fn C Buf -> (op1 (op.ret-const) (const-func Fn C) Buf))

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
                          (binary.put-u8 (op.tail-call) Buf)))

(define if-reg-expr
  Reg Else-Offset C Buf -> (op2 (op.jump-unless) Reg Else-Offset Buf))

(define push-error-handler
  [klvm.reg Reg] _ Buf -> (op1 (op.push-error-handler) Reg Buf))

(define pop-error-handler
  _ Buf -> (binary.put-u8 (op.pop-error-handler) Buf))

(define put-local-const-list'
  [] Buf -> Buf
  [[X Type I | Global] | Const] Buf -> (let Buf (const-ref Global Buf)
                                         (put-local-const-list' Const Buf)))

(define put-local-const-list
  Const Buf -> (let Cbuf (put-local-const-list' Const (binary.mkstream))
                    Buf (binary.put-u32 (binary.bytestream-size Cbuf) Buf)
                 (binary.put-bytestream Cbuf Buf)))

(define func-type-idx
  klvm.s1.func -> (klvm.bytecode.bin 00000000)
  klvm.s1.lambda -> (klvm.bytecode.bin 01000000)
  klvm.s1.toplevel -> (klvm.bytecode.bin 10000000))

(define func-header
  Type Name Arity Frame-size Frame-size+ C Buf ->
  (let Buf (binary.put-u8 (func-type-idx Type) Buf)
       Buf (binary.put-u16 Arity Buf)
       Buf (binary.put-u16 Frame-size Buf)
       Buf (binary.put-u16 Frame-size+ Buf)
    (put-str Name Buf)))

(define emit-func
  Type Name Args Frame-size Frame-size+ Code C Buf ->
  (let Arity (length Args)
       . (output "\\ emitting header~%")
       Buf (func-header Type Name Arity Frame-size Frame-size+ C Buf)
       . (output "\\ emitting local const list~%")
       Buf (put-local-const-list (klvm.bytecode.context-local-const C) Buf)
       . (output "\\ emitting code Code: ~S~%" Code)
       Buf (binary.put-bytestream Code Buf)
       . (output "\\ done emitting code Buf: ~S~%" Buf)
    Buf))

(define put-vec-const
  X Type-op Buf -> (put-str X (binary.put-u8 Type-op Buf)))

(define put-const
  X nil Buf -> (binary.put-u8 (type.nil) Buf)
  true bool Buf -> (binary.put-u8 (type.bool/true) Buf)
  false bool Buf -> (binary.put-u8 (type.bool/false) Buf)
  X func Buf -> (put-vec-const X (type.func) Buf)
  X lambda Buf -> (put-vec-const X (type.lambda) Buf)
  X str Buf -> (put-vec-const X (type.str) Buf)
  X sym Buf -> (put-vec-const X (type.sym) Buf)
  X vec Buf -> (put-vec-const X (type.vec))
  X i16 Buf -> (binary.put-u16 X (binary.put-u8 (type.i16) Buf))
  X i32 Buf -> (binary.put-u32 X (binary.put-u8 (type.i32) Buf))
  X i64 Buf -> (binary.put-u64 X (binary.put-u8 (type.i64) Buf)))

(define put-const-list
  [] Buf -> Buf
  [[X Type I | _] | Const] Buf -> (let Buf (put-const X Type Buf)
                                    (put-const-list Const Buf)))

(define module-header
  C -> (let Buf (binary.mkstream)
            Buf (binary.put-u32 (magic) Buf)
            Const (klvm.bytecode.context-global-const C)
            Cbuf (put-const-list Const (binary.mkstream))
            Buf (binary.put-u32 (binary.bytestream-size Cbuf) Buf)
         (binary.put-bytestream Cbuf Buf)))

(define prep-code
  C Code-buf -> (let Buf (module-header C)
                     Buf (binary.put-u32
                          (binary.bytestream-size Code-buf) Buf)
                     Buf (binary.put-bytestream Code-buf Buf)
                     . (output "~S: Code-buf: ~S~%" prep-code Code-buf)
                     . (output "~S: Buf ~S~%" prep-code Buf)
                  (binary.bytevector-from-bytestream Buf)))

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
