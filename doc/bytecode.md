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
All opcodes numbers >= 128. If a 

    nop
    a16 Uint1[2]
    a32 Uint1[4]
    b16 Uint1[1]
    b32 Uint1[3]
    To load-reg-> From
    To load-lambda-> X
    To load-const-> X
    jump +Off 
    X closure-lambda-> Nargs
    Reg closure-reg-> Nargs
    X closure-fn-> Nargs
    X closure-tail-lambda-> Nargs
    Reg closure-tail-reg-> Nargs
    X closure-tail-fn-> Nargs
    drop-ret
    load-ret-> Reg
    call
    tail-call
    +Off jump-unless Reg
    ret-reg Reg 
    ret-lambda X
    ret-const X
    push-error-handler Reg 
    pop-error-handler
    +
    -
    *
    /
    =
    >
    <
    >=
    <=
    /=

