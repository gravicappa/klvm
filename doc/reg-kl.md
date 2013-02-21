# Overview

reg-kl is a code translator that eliminates `let`, `lambda` constructions from
KL. It is useful for translating KL into languages that don't have sane
lambdas (Python, Java, C), closures (Java, C), have weird scope (js). Mind
that KL-code it emits requires further processing.

## Example:

This KL code


    (reg-kl-walk [[defun one [X]
                    [let Y [func1 X a]
                      [let Z [func2 X Y]
                        [let W [func3 Y Z]
                          [lambda A [lambda B [A B W]]]]]]]])


=>

    [[shen-mk-func one [X] 2
      [do [shen-set-reg! 0 [func1 [shen-get-arg 0] a]]
          [shen-set-reg! 1 [func2 [shen-get-arg 0] [shen-get-reg 0]]]
          [shen-set-reg! 1 [func3 [shen-get-reg 0] [shen-get-reg 1]]]
          [shen-mk-closure
            [A B]
            1
            [[shen-get-reg 1]]
            [[shen-get-arg 2] [shen-get-arg 1] [shen-get-arg 0]]]]]]

Each function or closure is represented as a procedure, required arguments and
a vector with closure variables. For a function closure vector is empty. If a
function are called with less number of arguments than required its vector
closure contains given variables. Python pseudocode explains it better.

    >>> def plus_fn(Args):
            if len(Args) < 2: # if arguments are not enough
                return [function_tag, plus_fn, 2, Args] # return closure
                return Args[0] + Args[1]

    >>> plus = [function_tag, plus_fn, 2, []]  # define function plus

    >>> call(plus, [1, 2])
    3

    >>> x = call(plus, [1])
    >>> x
    [function_tag, plus_fn, 2, [1]]  # closure with captured var

    >>> call(x, [2])                 # calling a closure with more var
    3

    >>> def adder_lambda_fn(Args):   # lifted lambda
            if len(Args) < 2:
                return [function_tag, adder_lambda_fn, 2, Args]
                return Args[0] + Args[1]

    >>> def adder_fn(Args):
            if len(Args) < 1:
                return [function_tag, adder_fn, 1, Args]
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

# Syntax

## [shen-mk-func Name Args Nregs Code]

Is obvious.

## [shen-set-reg! Reg Value]

Opcode for setting Value to register Reg.

## [shen-get-reg Reg]

Opcode for getting value of register Reg.

## [shen-get-arg Arg]

Opcode for getting value of argument Arg.

## [shen-mk-closure Args Nregs Init-list Code]

Opcode defining closure with arguments Args, initializing list for argument
registers, code.

## [shen-mk-freeze Nregs Init-list Code]

Opcode defining freeze with initializing list for argument registers and code.

# Known issues

## Translates `cond` into cascade of `if`:

    (reg-kl-walk [[defun func [A]
                    [cond [[= A 1] one]
                          [[= A 2] two]
                          [true many]]]])

=>

    [[shen-mk-func func [A]
       [if [= [shen-get-arg 0] 1]
           one
           [if [= [shen-get-arg 0] 2]
               two
               [if true
                   many
                   [error "error: cond failure"]]]]]]

## Information about variables names is lost.
