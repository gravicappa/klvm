# Structure

    module
      Magic: uint32 = 0x54e7XXXX where XXXX is a version number
      Const-list: const-list
      Func-list: func-list

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
      Flags: u8 | [00]: type ....
      Arity: u16
      Frame-size: u16
      Frame-size+: u16
      Name: vec
      Const: const-ref-list
      Code: vec

    const-ref-list
      Len: uint32
      Table: const-ref[]

    const-ref
      Idx: 

# Opcodes

    <program> := <opcode> <program>;
    <program> := <e>;
    <opcode> := <T1>;
    <opcode> := <op> <data>;
    <data> := <t1> <data>;
    <data> := <t2> <data>;
    <data> := <e>;

    <t1> := <t1.val>;
    <t1> := <+u8> XXXXXXXX <t1.val>;
    <t1> := <+u16> XXXXXXXX XXXXXXXX <t1.val>;
    <t1> := <+u32> XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX <t1.val>;
    <t1.val> := 00XXXXXX; # reg-> XXXXXX
    <t1.val> := 01XXXXXX; # ret-> XXXXXX

    <t2> := <t2.val>;
    <t2> := <+u8> XXXXXXXX <t2.val>;
    <t2> := <+u16> XXXXXXXX XXXXXXXX <t2.val>;
    <t2> := <+u32> XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX <t2.val>;
    <t2.val> := 00XXXXXX; # <-reg XXXXXX
    <t2.val> := 01XXXXXX; # <-const XXXXXX

All opcodes numbers >= 128. If a 

    00XXXXXX | reg-> XXXXXX | <-reg XXXXXX
    01XXXXXX | ret-> XXXXXX | <-const XXXXXX
    0x80 | nop
    0x81 | +u8 XXXXXXXX
    0x82 | +u16 XXXXXXXX XXXXXXXX
    0x83 | +u32 XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX
         | jump
         | drop-ret
         | tail-call
         | closure-lambda-> Nargs
         | closure-reg-> Nargs
         | closure-tail-lambda-> Nargs
         | closure-tail-reg-> Nargs
         | jump-unless Reg
         | push-error-handler
         | pop-error-handler
         | ret-reg
         | ret-const
         | + Y
         | - Y
         | * Y
         | / Y
         | = Y
         | > Y
         | < Y
         | >= Y
         | <= Y
         | /= Y
