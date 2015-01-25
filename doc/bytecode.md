1. [reg-> x y]
   [reg4-> x x x x y y y y]

2. [x y reg->]
   [u32 x x x x u32 y y y y reg->]

3. 

-------------------------------------------------
# Structure

    module
      Const-list: const-list
      Func-list: func-list
      Toplevel-code: vec

    const-list
      Size-bytes: uint32
      Consts: const[]

    const
      Type: uint1 (u16, u32, u64, sym, str, vec)
      Value:
        | uint16
        | uint32
        | uint64
        | vec

    vec
      Len: uint32
      Data: uint1[Len]

    func-list
      Size-bytes: uint32
      Func-list: func[]

    func 
      Name: vec
      Code: vec

# Opcodes

X  klvm.load-reg-> To From
X  klvm.load-lambda-> To X
X  klvm.load-const-> To X
X  klvm.jump +Off
N  klvm.closure-lambda-> X Nargs
XN klvm.closure-reg-> Reg Nargs
N  klvm.closure-fn-> X Nargs
N  klvm.closure-tail-lambda-> X Nargs
XN klvm.closure-tail-reg-> Reg Nargs
N  klvm.closure-tail-fn-> X Nargs
-  klvm.drop-ret
X  klvm.load-ret-> Reg
-  klvm.call
-  klvm.tail-call
XO klvm.jump-unless Reg +Off
X  klvm.ret-reg Reg
-  klvm.ret-fn X
-  klvm.ret-const X
X  klvm.push-error-handler Reg
-  klvm.pop-error-handler

X  klvm.+ A B
X  klvm.- A B
X  klvm.* A B
X  klvm./ A B
X  klvm.< A B
X  klvm.> A B
X  klvm.<= A B
X  klvm.>= A B

