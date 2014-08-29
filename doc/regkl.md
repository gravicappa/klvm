# Overview

`regkl` is a code translator that eliminates `let`, `lambda` forms from KL. It
is useful for translating KL into languages that don't have sane lambdas
(Python, Java, C), closures (Java, C), have weird scope (js). Mind that it
produces special REGKL code that is incompatible to KL.

## Example:

This KL code


    (regkl.walk [[defun one [X]
                   [let Y [func1 X a]
                     [let Z [func2 X Y]
                       [let W [func3 Y Z]
                         [lambda A [lambda B [A B W]]]]]]]]
                false)


is transformed into this REGKL code

    [[regkl.func one [X] 2 
      [do [regkl.reg-> 0 [func1 [regkl.arg 0] a]]
          [regkl.reg-> 1 [func2 [regkl.arg 0] [regkl.reg 0]]]
          [regkl.reg-> 1 [func3 [regkl.reg 0] [regkl.reg 1]]]
          [regkl.closure [A B] 0 [[regkl.reg 1]]
            [[regkl.arg 1] [regkl.arg 2] [regkl.arg 0]]]]]]

# Usage

    (regkl.walk Code Eliminate-toplevel-atoms?)

Where Code is a list of KL toplevel forms and boolean
Eliminate-toplevel-atoms? is self-explanatory. The latter is meant to be
`true` for general code translation and `false` for REPL targeted code
translation.

# Concept

Each function or closure is supposed to be represented as a vector containing
procedure, arity and a vector of free variables (*closure vector*). For a mere
function closure vector is empty. If a function is called with less number
of arguments than required then the closure vector of returned function object
contains given variables. Python pseudocode explains it better.

    >>> def plus_fn(*Args):
            # check if arguments are not enough
            if len(Args) < 2:
                # returning a closure
                return [function_tag, plus_fn, 2, Args]
            else:
                # returning the result
                return Args[0] + Args[1]

    >>> plus = [function_tag, plus_fn, 2, []]  # define function plus

    >>> call(plus, [1, 2])
    3

    >>> x = call(plus, [1])
    >>> x
    [function_tag, plus_fn, 2, [1]]  # closure with captured var

    >>> call(x, [2])                 # calling a closure with more var
    3

    >>> def adder_lambda_fn(*Args):   # lifted lambda
            if len(Args) < 2:
                return [function_tag, adder_lambda_fn, 2, Args]
            else:
                return Args[0] + Args[1]

    >>> def adder_fn(*Args):
            if len(Args) < 1:
                return [function_tag, adder_fn, 1, Args]
            else:
                return [function_tag, adder_lambda_fn, 2, [Args[0]]]

    >>> inc1 = call(adder_fn, [1])
    >>> inc1
    [function_tag, adder_lambda_fn, 2, [1]]

    >>> call(inc1, [5])
    6

    >>> inc2 = call(adder_fn, [2])
    [function_tag, adder_lambda_fn, 2, [2]]

    >>> call(inc2, [5])
    7

# REGKL forms

## [regkl.func Name Args Nregs Code]
## [regkl.toplevel Name Args Nregs Code]

Defines a function or a toplevel form `Name` with argument list `Args`, a
number of registers `Nregs` and a body `Code`. `Name` and `Args` are
superfluous for toplevel form but the former may be helpful for target
language translation and the latter is just for more unified look.

## [regkl.closure Args Nregs Init-list Code]

Defines a closure with arguments `Args`, number of registers `Nregs`,
initializing list for argument registers `Init-list`, and `Code`.

## [regkl.reg-> Reg Value]

Form that sets `Value` to register `Reg`.

## [regkl.reg Reg]

Form that returns value of register `Reg`.

## [regkl.arg Arg]

Form that returns value of a function argument `Arg`.

## [regkl.freeze Nregs Init-list Code]

Form that is similar to `regkl.closure` with `Args` being an empty list.

## [do A B ...]

Similar to `do` form in KL.

## [if If Then Else]

Similar to `if` form in KL.

# Known issues

## Translates `cond` into cascade of `if`:

    (regkl.walk [[defun func [A]
                   [cond [[= A 1] one]
                         [[= A 2] two]
                         [true many]]]]
                false)

=>

    [[regkl.func func [A] 0 
      [if [= [regkl.arg 0] 1]
          one
          [if [= [regkl.arg 0] 2]
              two
              [if true
                  many
                  [error "error: cond failure"]]]]]]

## Information about variables names is lost.
## Does not support `type` form yet.
