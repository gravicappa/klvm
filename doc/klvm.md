# KLVM Description

Virtual machine for KLVM consists of

  - contiguous register vector `reg`,
  - stack pointer `sp`,
  - special register for passing argument number `nargs`,
  - special register for passing a return value `ret`,
  - special register for passing code flow `next`,
  - special register for storing closure `t`,
  - stack of error handlers.

Register `t` is local to block it is used in. So it can be implemented in
target language as a local variable.

## Concept of KLVM

### Example implementation

KLVM virtual machine example (`test/stage-2.scm`) is implemented in [Gambit
Scheme](http://gambitscheme.org). It is made for testing purposes and as a
demonstration of KLVM principles. It is not an example of KLVM implementation
itself since **KLVM IS NOT MEANT TO BE INTERPRETED** but translated to target
language.


### Execution flow

Let's first look at this KLambda code:

    (defun func1 (A B)
      (let C (add A B)
        (let D (sqrt C) \\ for simplicity I'm avoiding tailcall here
          D)))

KLVM transformation can be outlined with assembler-like pseudocode shown
below. Also it demonstrate the usage of `nargs`, `next` and `ret` registers.

    func1.label0:
      stack[i] <- next
      A <- arg1
      B <- arg2
      nargs <- 2
      arg1 <- A
      arg2 <- B
      next <- func1.label1
      goto add.label0

    func1.label1:
      C <- ret
      nargs <- 1
      arg1 <- C
      next <- func1.label2
      goto sqrt.label0

    func1.label2:
      D <- ret \\ not omitted for clarity
      ret <- D \\ ...
      next <- stack[i]
      goto next

    add.label0:
      X <- arg1
      Y <- arg2
      ret <- (+ X Y)
      goto next

    sqrt.label0:
      X <- arg1
      ret <- (sqrt X)
      goto next

You can see that each function is divided into blocks (the code between
labels) which are meant to be executed atomically.

In [shen-py](https://github.com/gravicappa/shen-py) the simplified outline of
that code with the main manager function `run` will look something like this:

    def add_label0():
      ...
      ret = x + y
      return next

    def sqrt_label0():
      ...
      ret = math.sqrt(x)
      return next

    def func1_label0():
      ...
      return add_label0

    def func1_label1():
      ...
      return sqrt_label0

    def func1_label2():
      ...
      return next

    def run(pc):
      while pc:
        pc = pc()

### Processing function arguments and local variables

The structure of a function's frame which is the part of `reg` vector where
arguments and the local variables of the function are stored looks like this:

    [... ArgN ... Arg1 Var1 ... VarN Stored-next Stored-nargs ...]
         ↑                                                    ↑
         sp                                                   sp + func's stacksize

Please note that parameters passed to function are put in `reg` **IN REVERSE**
order. This simplifies partial application support.

### KLVM code

KLVM code has simple structure:

    [toplevel-expression
     [[0
       [level-1-expression [level-2-expression ...]
                           [level-2-expression ...]
                           ...]
       [level-1-expression [level-2-expression ...]
                           [level-2-expression ...]
                           ...]
       [level-1-expression ...]]
      [1
       [level-1-expression [level-2-expression ...]
                           [level-2-expression ...]
                           ...]
       [level-1-expression ...]]
      ...]]

Where toplevel-expression is a form defining a named function, an unnamed
function (which in KLVM still has an identifier), and a toplevel function.

## Forms

- klvm.func
- klvm.toplevel 
- klvm.closure 

- klvm.call
- klvm.closure->
- klvm.entry
- klvm.func-obj
- klvm.goto
- klvm.goto-next
- klvm.if
- klvm.if-nargs>0
- klvm.lambda
- klvm.nargs
- klvm.nargs-
- klvm.nargs->
- klvm.nargs-cond
- klvm.next
- klvm.next->
- klvm.nregs->
- klvm.pop-error-handler
- klvm.push-error-handler
- klvm.put-closure-args
- klvm.reg
- klvm.reg->
- klvm.ret
- klvm.ret->
- klvm.return
- klvm.runtime
- klvm.sp+
- klvm.sp-
- klvm.tailcall
- klvm.tailif
- klvm.thaw
- klvm.wipe

- klvm.entry-template
- klvm.return-template

### Toplevel forms

- klvm.func
- klvm.toplevel 
- klvm.closure 

### Level 1 forms

- klvm.call
- klvm.closure->
- klvm.entry
- klvm.goto
- klvm.goto-next
- klvm.if
- klvm.if-nargs>0
- klvm.nargs-
- klvm.nargs->
- klvm.nargs-cond
- klvm.next->
- klvm.nregs->
- klvm.pop-error-handler
- klvm.push-error-handler
- klvm.put-closure-args
- klvm.reg->
- klvm.ret->
- klvm.return
- klvm.runtime
- klvm.sp+
- klvm.sp-
- klvm.tailcall
- klvm.tailif
- klvm.thaw
- klvm.wipe

#### [klvm.entry Func Arity Name]

### Level 2 forms 

- klvm.reg
- klvm.ret
- klvm.next
- klvm.nargs
- klvm.func-obj
- klvm.lambda
- fail
- *constant atoms*

#### [klvm.reg Index]

Returns `reg[sp + Index]`.

#### [klvm.ret]

Returns the contents of `ret` register.

#### [klvm.next]

Returns the contents of `next` register.

#### [klvm.nargs]

Returns the contents of `nargs` register.

#### [klvm.func-obj Func Arity Name]

Creates and returns a function object of a function `Func` with `Arity` and
name `Name`. A `Name` can be [] representing anonymous function.

#### [klvm.lambda X]

Represents a pointer to anonymous function created by `[klvm.closure X ...]`.
It is introduced to detect the named function call from lambda call. In the
latter case target language translator may use direct lambda object instead of
making a function name lookup.

-----------------------------------

################

### klvm-closure, klvm-func, klvm-toplevel

    [klvm-closure Name Nargs Nregs Code]
    [klvm-func Name Nargs Nregs Code]
    [klvm-toplevel Name Nargs Nregs Code]

Define a lambda, function or toplevel with name `Name`, number of parameters
`Nargs`, number of used registers `Nregs` and code `Code`.

The `Code` is expected to be `[...]` form which has the following
structure:

    [[0 ...]
     [1 ...]
     [2 ...]
     ...
     [N ...]]

The order of labels is not important but label 0 must exist.

### klvm-return

[klvm-return]

Return from current code block. VM will run code pointed by register `next`.
See execution model.

### klvm-goto

[klvm-goto X]

Jump to label `X`.

### klvm-call

[klvm-call Funcptr]

Jump to function defined by level 2 expression `Funcptr` that returns function
pointer. Often occures with `klvm-closure-func` expression. 

[klvm-call [klvm-closure-func]]

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

[klvm-nregs-> [N]]
[klvm-nregs-> [N [klvm-closure-nargs]]]

Ensure that the length of the registers vector if not less that `sp + N` or
`sp + N + [klvm-closure-nargs]`. Otherwise either increase it or raise an
error.

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

[trap-error X [lambda E Y]]

to
[klvm-trap-error [freeze X] [lambda E Y]]

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
