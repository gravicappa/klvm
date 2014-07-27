It is a translator that transforms [Shen](http://shenlanguage.org)'s KLambda
to KLVM code which is to be translated further into target language or
compiled to bytecode. KLVM is best suitable for languages that support
imperative paradigm like C, Java, Go, Javascript, Python, PHP, TCL, etc.
KLVM takes care of

  - TCO,
  - partial application,
  - lambdas,
  - closures,
  - exception handling.

It consists of 4 translators: denest, regkl, stage-1, stage-2.

denest
------
Is just simply denests expressions. Like

    (f1 (f2 A B) (f3 C (f4 D E)) F)

into

    (let X1 (f2 A B)
      (let X2 (f4 D E)
        (let X3 (f3 C X2)
          (f1 X1 C3 F))))

It produces fully compatible KL.

regkl
------
regkl is a code translator that eliminates `let`, `lambda` forms from KL. It
is useful for translating KL into languages that don't have sane lambdas
(Python, Java, C), closures (Java, C), have weird scope (js). Mind that it
produces special `regkl` code that is incompatible to KL.

See `doc/regkl.md` for details.

KLVM
----
See `doc/klvm.md` for details.
