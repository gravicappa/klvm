(define klvm.s1.t1
  -> (let Kl (head (value klvm-test.*code1*))
          Klvm1 (klvm.s1.walk [] [Kl])
          . (output "~%*KLVM1:~%~S~%~%" Klvm1)
       true))

(define klvm.s1.t2
  -> (let Kl (value klvm-test.*code1*)
          Klvm1 (klvm.s1.walk [] Kl)
          . (backend-utils.with-file-output
             "klvm/test/code.klvm1"
             (/. F (pr (backend-utils.str-from-sexpr "~R~%" Klvm1) F)))
       true))

(define klvm.s1.freeze
  -> (let Kl [(klvm-test.find-func klvm-test.test-freeze)]
          . (output "~%*KL:~%~S~%~%" Kl)
          Klvm1 (klvm.s1.walk [] Kl)
          . (output "~%*KLVM1:~%~S~%~%" Klvm1)
       true))

(define klvm.regkl.t
  X -> (regkl.walk (map (function denest.walk) X) false))

(define klvm.s1-from-kl
  Kl -> (let Klvm1 (klvm.s1.walk [] Kl)
          Klvm1))

\*

(klvm.s1.walk [] [[defun X [X] [+ X 5]]])

(klvm.s1.walk [] [[defun test-freeze []
                    [let X 5
                      [let F [freeze [* X 5]]
                        [let X 6 [thaw F]]]]]])

(klvm.s1.walk [] [[defun adder [V10181]
                    [lambda X [+ V10181 X]]]])

(klvm.s1.walk [] [[defun test-tailremap [X Y Z]
                    [f1 Z Y X]]])

(klvm.regkl.t [[defun test-freeze []
                 [let X 5
                   [let F [freeze [* X 5]]
                     [let X 6 [thaw F]]]]]])

(klvm.regkl.t [[defun test-closure-4 []
                 [let X 5
                   [let F [lambda A [+ A X]]
                     [let G [lambda F [lambda X [F X]]]
                       [G F 12]]]]]])

(denest.walk [defun t4 []
               [let F [lambda A [+ A 5]]
                 [let G [lambda F [lambda X [F X]]]
                   [G F 12]]]])

(klvm.regkl.t [[defun t []
                 [trap-error [/ 1 0] [lambda E E]]]])

(regkl.used-vars [lambda A [lambda B [A B]]] [[A | 1] [B | 0]])

[[regkl.func test-closure-4 [] 2 [do [regkl.set-reg! 0 5] [regkl.set-reg! 0 [regkl.closure [A] 0 [[regkl.get-reg 0]] [+ [regkl.get-arg 1] [regkl.get-arg 0]]]] [regkl.set-reg! 1 [regkl.closure [F X] 0 [] [[regkl.get-arg 1] [regkl.get-arg 0]]]] [[regkl.get-reg 1] [regkl.get-reg 0] 12]]]]

(klvm.regkl.t [
[defun test-tail-call []
  [let X [adder 6]
    [let Y [X 8]
      [- Y 14]]]]])


*\
