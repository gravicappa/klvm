# KLVM Description

## KLVM operators

### klvm-call

    [klvm-call Obj]

### klvm-closure->

    [klvm-closure-> Obj]

### klvm-closure-func

    [klvm-closure-func]

### klvm-closure-nargs

    [klvm-closure-nargs]

### klvm-current-error
### klvm-dec-nargs
### klvm-dec-stack-ptr
### klvm-error-unwind-get-handler
### klvm-func-obj
### klvm-goto
### klvm-inc-nargs
### klvm-inc-stack-ptr
### klvm-label
### klvm-mk-closure
### klvm-nargs
### klvm-nargs->
### klvm-nargs>0
### klvm-nargs-cond
### klvm-nregs->
### klvm-pop-closure-args
### klvm-pop-error-handler
### klvm-pop-extra-args
### klvm-push-error-handler
### klvm-push-extra-args
### klvm-reg
### klvm-reg->
### klvm-return
### klvm-stack
### klvm-stack->
### klvm-stack-size
### klvm-thaw
### klvm-closure
### klvm-func
### klvm-toplevel

## Run loop
## Exception handling

    [trap-error X [lambda E Y]] ->
    [shen-trap-error [freeze X] [lambda E Y]] ->

## Sample
#### KLambda:

    [defun list-len-aux [List Acc]
      [if [cons? List]
          [list-len-aux [tl List] [+ Acc 1]]
          Acc]]

#### KLVM:

    [klvm-func list-len-aux [List Acc] 2
     [[klvm-label 0]
      [klvm-nargs-cond [[klvm-nregs-> [2]]
                        [klvm-reg-> [1] [klvm-func-obj]]
                        [klvm-return]]
                       [[klvm-dec-nargs 2]]
                       [[klvm-dec-nargs 2]
                        [klvm-stack-size [klvm-nargs]]
                        [klvm-push-extra-args [klvm-nargs]]
                        [klvm-inc-stack-ptr [klvm-nargs]]]]
      [klvm-nregs-> [6]]
      [klvm-stack-size 6]
      [klvm-stack-> 0 [klvm-nargs]]
      [klvm-stack-> 1 [klvm-reg 0]]
      [klvm-stack-> 2 [klvm-reg 1]]
      [klvm-stack-> 3 [klvm-reg 2]]
      [klvm-stack-> 4 [klvm-reg 3]]
      [klvm-stack-> 5 [klvm-reg 4]]
      [klvm-closure-> cons?]
      [klvm-nregs-> [2 [klvm-closure-nargs]]]
      [klvm-pop-closure-args]
      [klvm-reg-> [0] 1]
      [klvm-reg-> [1 [klvm-closure-nargs]] [klvm-stack 2]]
      [klvm-inc-stack-ptr 6]
      [klvm-nargs-> 1]
      [klvm-inc-nargs [klvm-closure-nargs]]
      [klvm-call [klvm-closure-func]]]
     [[klvm-label 1]
      [klvm-reg-> [3] [klvm-reg 1]]
      [klvm-dec-stack-ptr 6]
      [klvm-reg-> [0] [klvm-stack 1]]
      [klvm-reg-> [1] [klvm-stack 2]]
      [klvm-reg-> [2] [klvm-stack 3]]
      [klvm-reg-> [4] [klvm-stack 5]]
      [klvm-goto 2]]
     [[klvm-label 4]
      [klvm-stack-> 1 [klvm-reg 0]]
      [klvm-stack-> 2 [klvm-reg 1]]
      [klvm-stack-> 3 [klvm-reg 2]]
      [klvm-stack-> 4 [klvm-reg 3]]
      [klvm-stack-> 5 [klvm-reg 4]]
      [klvm-closure-> tl]
      [klvm-nregs-> [2 [klvm-closure-nargs]]]
      [klvm-pop-closure-args]
      [klvm-reg-> [0] 5]
      [klvm-reg-> [1 [klvm-closure-nargs]] [klvm-stack 2]]
      [klvm-inc-stack-ptr 6]
      [klvm-nargs-> 1]
      [klvm-inc-nargs [klvm-closure-nargs]]
      [klvm-call [klvm-closure-func]]]
     [[klvm-label 5]
      [klvm-reg-> [3] [klvm-reg 1]]
      [klvm-dec-stack-ptr 6]
      [klvm-reg-> [0] [klvm-stack 1]]
      [klvm-reg-> [1] [klvm-stack 2]]
      [klvm-reg-> [2] [klvm-stack 3]]
      [klvm-reg-> [4] [klvm-stack 5]]
      [klvm-stack-> 1 [klvm-reg 0]]
      [klvm-stack-> 2 [klvm-reg 1]]
      [klvm-stack-> 3 [klvm-reg 2]]
      [klvm-stack-> 4 [klvm-reg 3]]
      [klvm-stack-> 5 [klvm-reg 4]]
      [klvm-closure-> +]
      [klvm-nregs-> [3 [klvm-closure-nargs]]]
      [klvm-pop-closure-args]
      [klvm-reg-> [0] 6]
      [klvm-reg-> [1 [klvm-closure-nargs]] [klvm-stack 3]]
      [klvm-reg-> [2 [klvm-closure-nargs]] 1]
      [klvm-inc-stack-ptr 6]
      [klvm-nargs-> 2]
      [klvm-inc-nargs [klvm-closure-nargs]]
      [klvm-call [klvm-closure-func]]]
     [[klvm-label 6]
      [klvm-reg-> [4] [klvm-reg 1]]
      [klvm-dec-stack-ptr 6]
      [klvm-reg-> [0] [klvm-stack 1]]
      [klvm-reg-> [1] [klvm-stack 2]]
      [klvm-reg-> [2] [klvm-stack 3]]
      [klvm-reg-> [3] [klvm-stack 4]]
      [klvm-stack-> 1 [klvm-reg 0]]
      [klvm-stack-> 2 [klvm-reg 1]]
      [klvm-stack-> 3 [klvm-reg 2]]
      [klvm-stack-> 4 [klvm-reg 3]]
      [klvm-stack-> 5 [klvm-reg 4]]
      [klvm-nargs-> [klvm-stack 0]]
      [klvm-closure-> list-len-aux]
      [klvm-nregs-> [[klvm-nargs] 2 [klvm-closure-nargs]]]
      [klvm-pop-closure-args]
      [klvm-reg-> [1 [klvm-closure-nargs]] [klvm-stack 4]]
      [klvm-reg-> [2 [klvm-closure-nargs]] [klvm-stack 5]]
      [klvm-dec-stack-ptr [klvm-nargs]]
      [klvm-pop-extra-args [klvm-nargs]]
      [klvm-inc-nargs 2]
      [klvm-inc-nargs [klvm-closure-nargs]]
      [klvm-call [klvm-closure-func]]]
     [[klvm-label 7]
      [klvm-nargs-> [klvm-stack 0]]
      [klvm-nargs>0 [[klvm-nregs-> [[klvm-nargs]]]
                     [klvm-dec-stack-ptr [klvm-nargs]]
                     [klvm-pop-extra-args [klvm-nargs]]
                     [klvm-call [klvm-reg 2]]]
                    [[klvm-reg-> [1] [klvm-reg 2]]
                     [klvm-return]]]]
     [[klvm-label 2]
      [if [klvm-reg 3]
          [klvm-goto 4]
          [klvm-goto 7]]]]
