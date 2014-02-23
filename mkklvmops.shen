#!/usr/bin/env shen_run_nc

(define newline
  F -> (pr (make-string "~%") F))

(define write-ops-nums
  [] _ F -> (pr "    X -> (error c#34;Unknown operation ~A.c#34; X)" F)
  [Op | Ops] I F -> (do (pr (make-string "    op_~A -> ~A~%" Op I) F)
                        (write-ops-nums Ops (+ I 1) F)))

(define output-file
  Name Ops -> (let F (open Name out)
                   . (pr "(package klvm []" F)
                   . (newline F)
                   . (pr "  (define opcode-num" F)
                   . (newline F)
                   . (write-ops-nums Ops 128 F)
                   . (pr "))" F)
                   . (close F)
                true))

(define main
  _ -> (output-file "klvm-ops.shen" (read-file "klvm_ops")))
