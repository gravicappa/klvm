# KLVM Description

VM uses

  - stack,
  - stack pointer,
  - registers vector,
  - `nargs` special register,
  - stack of error handlers.

## Calling a function

Parameters passed to the function are put in registers vector starting from
index 1. At index 0 stored next label where VM jumps after calling function.

## Structure of KLVM code

  [toplevel-expression
   [[klvm-label 0]
    [level-1-expression
     [level-2-expression ...]
     ...]
    [level-1-expression
     [level-2-expression ...]
     ...]
    [level-1-expression ...]]
   [[klvm-label N]
    [level-1-expression ...]
    [level-1-expression ...]]
   ...]

### toplevel operations

- klvm-closure
- klvm-func
- klvm-toplevel  
- klvm-nargs->  
- klvm-call

`klvm-call` and `klvm-nargs->` are actually level 1 operations that can be in
toplevel.

#### level 1 operations

- klvm-return
- klvm-inc-nargs
- klvm-dec-nargs
- klvm-stack-size
- klvm-nregs->
- klvm-stack->
- klvm-reg->
- klvm-inc-stack-ptr
- klvm-dec-stack-ptr
- klvm-nargs->
- klvm-goto
- klvm-call
- klvm-push-error-handler
- klvm-pop-error-handler
- klvm-nargs-cond
- klvm-nargs>0
- klvm-push-extra-args
- klvm-pop-extra-args
- klvm-put-closure-args
- klvm-if
- klvm-closure->

#### level 2 operations 

- klvm-closure-nargs
- klvm-closure-func
- klvm-func-obj
- klvm-reg
- klvm-stack
- klvm-nargs
- klvm-mk-closure
- klvm-error-unwind-get-handler
- klvm-current-error
- fail
- *constant atoms*

## KLVM operators

For simplicity items starting with `X2-` prefix are level 2 operations, `X1-`
are level 1 operations, `C-` are constants.

### klvm-call

    [klvm-call X2-func]

Jump to function `X2-func`.

### klvm-closure->

    [klvm-closure-> Obj]

Store closure object `Obj` in a special register or local variable for further
use.

### klvm-closure-func

    [klvm-closure-func]

Return pointer to function of a stored by `klvm-closure->` closure.

### klvm-closure-nargs

    [klvm-closure-nargs]

Return number of arguments of a stored by `klvm-closure->` closure.

### klvm-current-error

    [klvm-current-error]

Return current error object.

### klvm-inc-nargs

    [klvm-inc-nargs Increment]

Increase the value of `nargs` register by `Increment`.

### klvm-dec-nargs

    [klvm-dec-nargs Decrement]

Decrease the value of `nargs` register by `Decrement`.

### klvm-inc-stack-ptr

    [klvm-inc-stack-ptr Increment]

Increase stack pointer by `Increment`.

### klvm-dec-stack-ptr

    [klvm-dec-stack-ptr Decrement]

Decrease stack pointer by `Decrement`.

### klvm-error-unwind-get-handler

    [klvm-error-unwind-get-handler]

Unwind stack to error handler scope and returns 1-place error handler
function.

### klvm-func-obj

    [klvm-func-obj]

Return current function object.

### klvm-nargs

    [klvm-nargs]

Return the value of `nargs` register.

### klvm-nargs->

    [klvm-nargs-> X]

Sets the value to `nargs` register to `X`.

### klvm-nargs>0

    [klvm-nargs>0
      A
      B]

If the value of `nargs` register > 0 execute KLVM expressions `A`, else
execute `B`.

### klvm-nargs-cond

    [klvm-nargs-cond
      A
      B
      C]

If the value of `nargs` register is less than number of function's parameters
execute KLVM expression `A`. If the value of `nargs` register is equal to
number of parameters then execute `B`. And if the value of `nargs` is greater
than number of parameters then execute `C`.

### klvm-nregs->

    [klvm-nregs-> [A | B]]

Ensure that the length of the registers vector is not less than the sum of
values in list `[A | B]`.

### klvm-reg

    [klvm-reg X]

Return the contents of register number `X`.

### klvm-reg->

    [klvm-reg-> [X | Y] V]

Set the value `V` to register number of sum of values in list `[X | Y]`.

### klvm-stack

    [klvm-stack X]

Return the contents of stack with the offset `X` from stack pointer.

### klvm-stack->

    [klvm-stack-> X Y]

Sets the contents of stack item with the offset `X` from stack pointer to the
value `Y`.

### klvm-put-closure-args

    [klvm-put-closure-args]

Puts closure parameters to registers vector starting from index 1.

### klvm-push-error-handler

    [klvm-push-error-handler E]

Push error handler `E`, stack pointer, `nargs` register, and register 0 into
error handlers stack.

### klvm-pop-error-handler

    [klvm-pop-error-handler]

Pop top element from error handlers stack.

### klvm-push-extra-args

    [klvm-push-extra-args]

Push `nargs` arguments registers vector beginning from index 1 to stack.

### klvm-pop-extra-args

    [klvm-pop-extra-args]

Pop `nargs` arguments from stack to registers vector beginning from index 1.

### klvm-return

    [klvm-return]

Return from current code block. VM will run next taken from register 0.

### klvm-stack-size

    [klvm-stack-size X]

Ensure that stack size if not less that `X`. Otherwise either increase stack
size or raise an error.

### klvm-goto

    [klvm-goto X]

Jump to label X.

### klvm-label

    [[klvm-label X]
     ...
     ...]

Define label `X`.

### klvm-mk-closure

    [klvm-mk-closure Function Nargs Ninit]

Create closure object with function pointer `Function`, number of parameters
`Nargs`, and number of closure values `Ninit`. Closure values are taken from
stack.

### klvm-closure, klvm-func, klvm-toplevel

  [klvm-closure Name Nargs Nregs Code]
  [klvm-func Name Nargs Nregs Code]
  [klvm-toplevel Name Nargs Nregs Code]

Define a lambda, function or toplevel with name `Name`, number of parameters
`Nargs`, number of used registers `Nregs` and code `Code`.

The `Code` must have the following structure:

    [[[klvm-label 0]
      ...]
     [[klvm-label 1]
      ...]
     ...]

The order of labels is not important but label 0 must exist.

### klvm-if

    [klvm-if Cond-expr Then-expr Else-expr]

If the value of `Cond-expr` is true then execute `Then-expr` else execute
`Else-expr`. `Cond-expr` is level 2 operation, `Then-expr` and `Else-expr` are
level 1 operations.

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
      [klvm-put-closure-args]
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
      [klvm-put-closure-args]
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
      [klvm-put-closure-args]
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
      [klvm-put-closure-args]
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
