# KLVM Description

## Run loop
## Exception handling

    [trap-error X [lambda E Y]] ->
    [shen-trap-error [freeze X] [lambda E Y]] ->

## KLVM operators
## Evaluator

# KLVM patterns

## Call (func : symbol)

    [klvm-stack-> X [klvm-reg Y]] ...
    [klvm-nregs-> [M]]
    [klvm-reg-> [X] [klvm-stack Z]] ...
    [klvm-inc-stack-ptr N]
    [klvm-nargs-> M]
    [klvm-call func]

## Call (func : obj)

    [klvm-stack-> X [klvm-reg Y]] ...
    [klvm-tvar-> [klvm-closure-nargs func]]
    [klvm-nregs-> [M] [klvm-tvar]]
    [klvm-pop-closure-args func]
    [klvm-reg-> [X] [klvm-stack Z] [klvm-tvar]] ...
    [klvm-inc-stack-ptr N]
    [klvm-nargs-> M]
    [klvm-inc-nargs [klvm-tvar]]
    [klvm-call func]

## Entry

    [klvm-nargs-cond
     [[klvm-nregs-> [2]]
      [klvm-reg-> [1] [klvm-func-obj]]
      [klvm-return]]
     [[klvm-dec-nargs N]]
     [[klvm-dec-nargs N]
      [klvm-ensure-stack-size [klvm-nargs]]
      [klvm-push-extra-args [klvm-nargs]]
      [klvm-inc-stack-ptr [klvm-nargs]]]]

## Tailcall (func : symbol)

    [klvm-nargs-> [klvm-stack 0]]
    [klvm-reg-> [X] [klvm-stack Y]] ... \* push arguments *\
    [klvm-nregs-> [[klvm-nargs] N]]
    [klvm-dec-stack-ptr [klvm-nargs]]
    [klvm-pop-extra-args [klvm-nargs]]
    [klvm-inc-nargs N]
    [klvm-call func]

## Tailcall (func : obj)

    [klvm-nargs-> [klvm-stack 0]]
    [klvm-tvar-> [klvm-closure-nargs func]]
    [klvm-nregs-> [[klvm-nargs] N [klvm-tvar]]]
    [klvm-pop-closure-args func]
    [klvm-reg-> [X] [klvm-stack Y [klvm-tvar]]] ... \* push arguments *\
    [klvm-dec-stack-ptr [klvm-nargs]]
    [klvm-pop-extra-args [klvm-nargs]]
    [klvm-inc-nargs N]
    [klvm-inc-nargs [klvm-tvar]]
    [klvm-call func]

## Return

    [klvm-nargs-> [klvm-stack 0]]
    [klvm-nargs>0
     [[klvm-nregs-> [[klvm-nargs]]]
      [klvm-dec-stack-ptr [klvm-nargs]]
      [klvm-pop-extra-args [klvm-nargs]]
      [klvm-call [klvm-reg Retval]]]
     [[klvm-reg-> [1] Retval]
      [klvm-return]]]

## Store closure

    [klvm-nregs-> [M]]
    [klvm-reg-> [X] Y] ... \* translate init list *\
    [klvm-reg-> [Z] [klvm-mk-closure M F]]
    [klvm-stack-> [Z+1] [klvm-reg [Z]]]

## Return closure

    [klvm-nregs-> [M]]
    [klvm-reg-> [X] Y] ... \* translate init list *\
    [klvm-reg-> [1] [klvm-mk-closure M F]]
