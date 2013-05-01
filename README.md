It translates [Shen](http://shenlanguage.org)'s KL to KLVM code which is to be
translated further into target language or compiled to bytecode. It takes care
of

  - TCO,
  - partial application,
  - lambdas,
  - closures,
  - exception handling.

It consists of 3 translators: deinline-expr, reg-kl, klvm-trans.

deinline-expr
-------------
_to be written_

reg-kl
------
reg-kl is a code translator that eliminates `let`, `lambda` constructions from
KL. It is useful for translating KL into languages that don't have sane
lambdas (Python, Java, C), closures (Java, C), have weird scope (js). Mind
that KL-code it emits requires further processing.

KLVM
----
_to be written_

### Samples

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

### Opcodes
_to be written_

### Translating tips
_to be written_
