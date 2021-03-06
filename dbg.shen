(package klvm.dbg [klvm.func klvm.closure klvm.toplevel]

(define pp-code
  [] Out Sep -> true
  [X | Y] Out Sep -> (do (pr (make-string "~A~R" Sep X) Out)
                         (pp-code Y Out (make-string "~%    "))))

(define pp-label
  [N | Code] Out Sep -> (do (pr (make-string "~Ac#40;~A~%" Sep N) Out)
                            (pp-code Code Out "    ")
                            (pr "c#41;" Out)))

(define pp-labels'
  [[N | X]] Out Sep -> (pp-label [N | X] Out Sep)
  [[N | X] | Labels] Out Sep -> (let Sep' (make-string "~%   ")
                                     . (pp-label [N | X] Out Sep)
                                  (pp-labels' Labels Out Sep')))

(define pp-labels
  X Out -> (do (pp-labels' X Out "  c#40;")
               (pr "c#41;" Out)))

(define pretty-print-klvm
  [] Out -> (do (pr "c#10;" Out)
                true)
  [[Head Name Args Nregs Code] | Y] Out ->
  (let . (pr (make-string "c#40;~A ~R ~R ~R~%" Head Name Args Nregs) Out)
       . (pp-labels Code Out)
       . (pr (make-string "c#41;~%~%") Out)
    (pretty-print-klvm Y Out))
  where (element? Head [klvm.func klvm.toplevel klvm.closure])
  [X | Y] Out -> (let S (value *maximum-print-sequence-size*)
                      . (set *maximum-print-sequence-size* -1)
                      T1 (pr (make-string "~S~%" X) Out)
                      . (set *maximum-print-sequence-size* S)
                     (pretty-print-klvm Y Out)))

(define show-code
  X -> (pretty-print-klvm X (stoutput))))
