# KLVM Description

VM consists of

  - contiguous register vector `reg`,
  - stack pointer `sp`,
  - special register for passing argument number `nargs`,
  - special register for passing code flow `next`,
  - special register for storing closure `t`,
  - special register for storing sp `savesp`,
  - stack of error handlers.

Logically registers `t` and `savesp` are local to label they are used in. So
they can be implemented as local variables.

## Calling a function

Parameters passed to function are put in `reg` **IN REVERSE** order. The
number of parameters is stored in `nargs` and in `next` stored next label
where VM jumps after calling function.

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

Operators:

- klvm-func
- klvm-closure
- klvm-toplevel
- klvm-return
- klvm-goto
- klvm-call
- klvm-nargs->
- klvm-nargs
- klvm-inc-nargs
- klvm-dec-nargs
- klvm-nargs-cond
- klvm-if-nargs>0
- klvm-closure->
- klvm-closure-func
- klvm-closure-nargs
- klvm-put-closure-args
- klvm-wipe-stack
- klvm-restore-stack-ptr
- klvm-dec-stack-ptr
- klvm-nregs->
- klvm-current-error
- klvm-label
- klvm-if
- klvm-inc-stack-ptr
- klvm-save-stack-ptr
- klvm-next->
- klvm-next
- klvm-reg->
- klvm-reg
- klvm-error-unwind-get-handler
- klvm-pop-error-handler
- klvm-push-error-handler
- klvm-func-ptr
- klvm-func-obj

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
- klvm-goto
- klvm-call
- klvm-nargs->
- klvm-inc-nargs
- klvm-dec-nargs
- klvm-nargs-cond
- klvm-if-nargs>0
- klvm-closure->
- klvm-put-closure-args
- klvm-nregs->
- klvm-if
- klvm-wipe-stack
- klvm-inc-stack-ptr
- klvm-dec-stack-ptr
- klvm-save-stack-ptr
- klvm-restore-stack-ptr
- klvm-next->
- klvm-reg->
- klvm-push-error-handler
- klvm-pop-error-handler

#### level 2 operations 

- klvm-closure-func
- klvm-closure-nargs
- klvm-func-obj
- klvm-reg
- klvm-nargs
- klvm-next
- klvm-func-ptr
- fail
- *constant atoms*

## KLVM operators

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

### klvm-label

    [[klvm-label X]
     ...
     ...]

Define label `X`.

### klvm-return

    [klvm-return]

Return from current code block. VM will run code pointed by register `next`.
See execution model.

### klvm-goto

    [klvm-goto X]

Jump to label `X`.

### klvm-call

    [klvm-call func]

Jump to function defined by level 2 expression `func`.

### klvm-nargs->

    [klvm-nargs-> X]

Sets the value to `nargs` register to `X`.

### klvm-inc-nargs

    [klvm-inc-nargs Increment]

Increase the value of `nargs` register by `Increment`.

### klvm-dec-nargs

    [klvm-dec-nargs Decrement]

Decrease the value of `nargs` register by `Decrement`.

### klvm-nargs-cond

    [klvm-nargs-cond Nargs A B C]

If the value of `Nargs` KLVM expression is less than number of function's
parameters execute KLVM expression `A`. If the value of `nargs` register is
equal to number of parameters then execute `B`. And if the value of `nargs` is
greater than number of parameters then execute `C`. The implementation may
choose not to inline the expansion of this operator but use separate primitive
translated from `klvm-func-entry-tpl` template.

### klvm-if-nargs>0

    [klvm-if-nargs>0 X Next A B]

If the value of `nargs` register > 0 execute KLVM expressions `A`, else
execute `B`. Arguments `X` and `Next` are used if implementation chooses not
to inline the expansion of this operator but use separate primitive translated
from `klvm-func-return-tpl` template.

### klvm-closure->

    [klvm-closure-> Obj]

Store closure object defined by level 2 expression `Obj` in temporary register
`t`.

### klvm-put-closure-args

    [klvm-put-closure-args Off]

Puts closure variables to registers vector starting from index `Off`.

### klvm-nregs->

    [klvm-nregs-> [A | B]]

Ensure that the length of the registers vector if not less that
`sp + [A | B]`. Otherwise either increase it or raise an error.

### klvm-if

    [klvm-if Cond-expr Then-expr Else-expr]

If the value of `Cond-expr` is true then execute `Then-expr` else execute
`Else-expr`. `Cond-expr` is level 2 operation, `Then-expr` and `Else-expr` are
level 1 operations.

### klvm-wipe-stack

    [klvm-wipe-stack Off]

Clears registers vector from `sp + Off` till its top used index.

### klvm-inc-stack-ptr

    [klvm-inc-stack-ptr Increment]

Increase the value of `sp` register by `Increment`.

### klvm-dec-stack-ptr

    [klvm-dec-stack-ptr Decrement]

Decreace the value of `sp` register by `Increment`.

### klvm-save-stack-ptr
    
    [klvm-save-stack-ptr]

Stores `sp` in temporary register `savesp`.

### klvm-restore-stack-ptr

    [klvm-restore-stack-ptr]

Restores `sp` from temporary register `savesp`.

### klvm-next->

    [klvm-next-> X]

Sets the contents of the `next` register with the pointer to label `X` code.

### klvm-reg->

    [klvm-reg-> X V]

Set the value `V` to register number `sp + X`.

### klvm-push-error-handler

    [klvm-push-error-handler E]

Push error handler `E`, `sp`, `nargs`, `next` registers into error handlers
stack.

### klvm-pop-error-handler

    [klvm-pop-error-handler]

Pop top element from error handlers stack.

### klvm-closure-func

    [klvm-closure-func]

Return pointer to function of closure in temporary register `t`.

### klvm-closure-nargs

    [klvm-closure-nargs]

Return number of arguments closure in temporary register `t`.

### klvm-func-obj

    [klvm-func-obj]

Return current function object.

### klvm-reg

    [klvm-reg X]

Return the contents of register number `sp + X`.

### klvm-nargs

    [klvm-nargs]

Return the value of `nargs` register.

### klvm-next

    [klvm-next]

Return the value of `next` register.

### klvm-func-ptr

    [klvm-func-ptr F]

Return func pointer of a function with name `F`, where `F` is a symbol.

### fail

    [fail]

Return fail object.

## Execution model

Scheme-like pseudocode:

    (define (run-loop-aux x)
      (if (procedure? x)
          (run-loop-aux x)
          #f))

    (define (run-loop x)
      (if (procedure? x)
          (run-loop (with-exception-handler
                     (lambda () (run-loop-aux x))
                     (lambda (e)
                       (let ((eh (klvm-pop-error-handler)))
                         (klvm-closure-> (error-handler-fn eh))
                         (klvm-nargs 1)
                         (klvm-reg-size (+ 1 (klvm-closure-nargs)))
                         (klvm-reg 0 e)
                         (klvm-put-closure-args 1)
                         eh))))
          #f))

## Exception handling

KLVM translates

    [trap-error X [lambda E Y]] ->

to
    [klvm-trap-error [freeze X] [lambda E Y]] ->

Where `klvm-trap-error` definition is provided by `klvm-runtime` function.

## Shen functions that must be provided by implementation

### klvm-mk-closure

    [klvm-mk-closure Function Args]

This function must exists in KLVM-based Shen. It creates closure object with
function pointer `Function`, and closure variables with values `Args`.

## Sample
#### KLambda:

    [defun list-len-aux [List Acc]
      [if [cons? List]
          [list-len-aux [tl List] [+ Acc 1]]
          Acc]]

#### KLVM:

    [klvm-func list-len-aux [List Acc] 2
     [[klvm-label 0]
      [klvm-nargs-cond
       2
       [[klvm-nregs-> [1]]
        [klvm-reg-> 0 [klvm-func-obj list-len-aux 2]]
        [klvm-wipe-stack 1]
        [klvm-return [klvm-next]]]
       [[klvm-dec-nargs 2]]
       [[klvm-inc-stack-ptr [klvm-nargs]]
        [klvm-dec-stack-ptr 2]
        [klvm-dec-nargs 2]]]
      [klvm-nregs-> [6]]
      [klvm-reg-> 4 [klvm-nargs]]
      [klvm-reg-> 5 [klvm-next]]
      [klvm-closure-> cons?]
      [klvm-next-> 1]
      [klvm-nregs-> [7 [klvm-closure-nargs]]]
      [klvm-nargs-> 1]
      [klvm-reg-> 6 [klvm-reg 1]]
      [klvm-put-closure-args 6]
      [klvm-inc-stack-ptr 6]
      [klvm-inc-nargs [klvm-closure-nargs]]
      [klvm-call [klvm-closure-func]]]
     [[klvm-label 1]
      [klvm-dec-stack-ptr 6]
      [klvm-reg-> 2 [klvm-reg 6]]
      [klvm-wipe-stack 6]
      [klvm-goto 2]]
     [[klvm-label 4]
      [klvm-closure-> tl]
      [klvm-next-> 5]
      [klvm-nregs-> [7 [klvm-closure-nargs]]]
      [klvm-nargs-> 1]
      [klvm-reg-> 6 [klvm-reg 1]]
      [klvm-put-closure-args 6]
      [klvm-inc-stack-ptr 6]
      [klvm-inc-nargs [klvm-closure-nargs]]
      [klvm-call [klvm-closure-func]]]
     [[klvm-label 5]
      [klvm-dec-stack-ptr 6]
      [klvm-reg-> 2 [klvm-reg 6]]
      [klvm-wipe-stack 6]
      [klvm-closure-> +]
      [klvm-next-> 6]
      [klvm-nregs-> [8 [klvm-closure-nargs]]]
      [klvm-nargs-> 2]
      [klvm-reg-> 6 1]
      [klvm-reg-> 7 [klvm-reg 0]]
      [klvm-put-closure-args 6]
      [klvm-inc-stack-ptr 6]
      [klvm-inc-nargs [klvm-closure-nargs]]
      [klvm-call [klvm-closure-func]]]
     [[klvm-label 6]
      [klvm-dec-stack-ptr 6]
      [klvm-reg-> 3 [klvm-reg 6]]
      [klvm-wipe-stack 6]
      [klvm-closure-> list-len-aux]
      [klvm-nargs-> [klvm-reg 4]]
      [klvm-dec-stack-ptr [klvm-nargs]]
      [klvm-save-stack-ptr]
      [klvm-inc-stack-ptr [klvm-nargs]]
      [klvm-next-> [klvm-reg 5]]
      [klvm-nargs-> 2]
      [klvm-inc-nargs [klvm-reg 4]]
      [klvm-nregs-> [2]]
      [klvm-reg-> 0 [klvm-reg 3]]
      [klvm-reg-> 1 [klvm-reg 2]]
      [klvm-nregs-> [2 [klvm-closure-nargs]]]
      [klvm-put-closure-args 0]
      [klvm-inc-nargs [klvm-closure-nargs]]
      [klvm-restore-stack-ptr]
      [klvm-call [klvm-closure-func]]]
     [[klvm-label 7]
      [klvm-nargs-> [klvm-reg 4]]
      [klvm-if-nargs>0
       [klvm-reg 0]
       [klvm-reg 5]
       [[klvm-closure-> [klvm-reg 0]]
        [klvm-nregs-> [[klvm-nargs] [klvm-closure-nargs]]]
        [klvm-next-> [klvm-reg 5]]
        [klvm-wipe-stack 0]
        [klvm-put-closure-args 0]
        [klvm-dec-stack-ptr [klvm-nargs]]
        [klvm-call [klvm-closure-func]]]
       [[klvm-reg-> 0 [klvm-reg 0]]
        [klvm-next-> [klvm-reg 5]]
        [klvm-wipe-stack 1]
        [klvm-return [klvm-next]]]]]
     [[klvm-label 2]
      [klvm-if [klvm-reg 2]
               [klvm-goto 4]
               [klvm-goto 7]]]]
