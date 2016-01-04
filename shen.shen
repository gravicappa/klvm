(set shen-kl-dir "official/Shen 19.2/KLambda")

(define shen-sources.kl
  Dir -> (let Shen-files ["core.kl" "declarations.kl" "load.kl" "macros.kl"
                          "prolog.kl" "reader.kl" "sequent.kl" "sys.kl"
                          "t-star.kl" "toplevel.kl" "track.kl" "types.kl"
                          "writer.kl" "yacc.kl"]
           (map (/. X (make-string "~A/~A" Dir X)) Shen-files)))

(define compile-shen-bin
  Dst -> (let Files (shen-sources.kl (value shen-kl-dir))
           (binary.bytevector-to-file
            Dst (klvm.bytecode.bin.from-files Files 2))))

(define compile-shen-asm
  Dst -> (let Files (shen-sources.kl (value shen-kl-dir))
           (klvm.bytecode.asm.print-to-file
            Dst (klvm.bytecode.asm.from-files Files 2))))
