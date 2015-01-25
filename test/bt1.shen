(define klvm.asm.t2
  -> (let Kl (value klvm-test.*code1*)
          Klvm1 (klvm.bytecode.asm.walk Kl 3)
          . (backend-utils.with-file-output
             "klvm/test/code.aklvm"
             \\(/. F (pr (backend-utils.str-from-sexpr "~R~%" Klvm1) F))
             (klvm.bytecode.asm.print Klvm1)
             )
       true))

(define klvm.asm.for-each
  Fn [] -> []
  Fn [X | Xs] -> (do (Fn X) (klvm.asm.for-each Fn Xs)))

(define klvm.asm.compile
  From To -> (let Kl (read-file From)
                  . (output "\\ compiling ~A~%" From)
                  S1 (klvm.s1.walk [] Kl)
                  . (backend-utils.with-file-output
                      (cn To ".s1")
                      (/. F (pr (backend-utils.str-from-sexpr "~R~%" S1) F)))
                  Klvm (klvm.bytecode.asm.walk Kl 3)
                  . (output "\\ saving ~A~%~%" To)
               (backend-utils.with-file-output
                To
                (klvm.bytecode.asm.print Klvm))))

(define klvm.asm.shen-compile
  -> (let Sdir "official/Shen 16/K Lambda"
          Ddir "klvm/test/shen"
          Files ["core.kl" "declarations.kl" "load.kl" "macros.kl" "prolog.kl"
                 "reader.kl" "sequent.kl" "sys.kl" "t-star.kl" "toplevel.kl"
                 "track.kl" "types.kl" "writer.kl" "yacc.kl"]
       (klvm.asm.for-each (/. X (klvm.asm.compile
                                 (make-string "~A/~A" Sdir X)
                                 (make-string "~A/~A.klvm.asm" Ddir X)))
                          Files)))


(define klvm.asm.shen-compile1
  -> (let Sdir "official/Shen 16/K Lambda"
          Ddir "klvm/test/shen"
          File "reader.kl"
       (klvm.asm.compile
        (make-string "~A/~A" Sdir File)
        (make-string "~A/~A.klvm.asm" Ddir File))))
