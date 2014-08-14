(define klvm.s2.t1
  -> (let Kl (head (value klvm-test.*code1*))
          Klvm1 (klvm.s1.walk [] [Kl])
          Klvm2 (klvm.s2.walk Klvm1)
          . (output "~%*KLVM2:~%~S~%~%" Klvm2)
          . (klvm.dbg.show-code Klvm2)
       true))

(define klvm.s2.t2
  -> (let Kl (value klvm-test.*code1*)
          Klvm1 (klvm.s1.walk [] Kl)
          Klvm2 (klvm.s2.walk Klvm1)
          . (backend-utils.with-file-output
             "klvm/test/code.klvm2"
             (klvm.dbg.pretty-print-klvm Klvm2))
       true))

(define klvm.s2.from-kl
  Kl -> (do (klvm.dbg.show-code (klvm.s2-from-kl [] Kl))
            true))

\*

(klvm.s2.from-kl  [[defun klvm.handle-error []]])
(klvm.s2.from-kl  [[defun func [] [+ 5 7]]])

*\
