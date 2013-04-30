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

### Opcodes
_to be written_

### Translating tips
_to be written_
