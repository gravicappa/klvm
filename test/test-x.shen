(package klvm.test.x [klvm.test.str-join klvm.test.list-from-vec
                      klvm.test.str-from-sexpr 
                      
                      klvm.s1.translate
                      klvm.s2.translate
                      klvm.func klvm.closure klvm.toplevel
                      walk]
  (define write
    (@p Code Test) Prefix -> 
    (let Defs (cn Prefix ".test")
         S1 (cn Prefix ".klvm1")
         S2 (cn Prefix ".klvm2")
         Sa (cn Prefix ".klvma")
         Sb (cn Prefix ".klvmb")
         . (write-to-file Defs (defs Test []))
         . (write-to-file S1 (s1 Code))
         . (write-to-file S2 (s2 Code))
         . (write-to-file Sa ";; asmc#10;")
         . (write-to-file Sb ";; bytecodec#10;")
      [Defs S1 S2 Sa Sb]))

  (define s1
    Code -> (let S1 (klvm.s1.translate [] Code true)
              (klvm.test.str-from-sexpr "~R~%" S1)))

  (define s2-prim _ -> walk)

  (define s2
    Code -> (let S1 (klvm.s1.translate [] Code true)
                 S2 (klvm.s2.translate S1)
               (str-s2 S2)))

  (define asm Code -> "")

  (define bytecode Code -> "")

  (define fmt-vec
    V -> (let L (map (function fmt) (klvm.test.list-from-vec V))
           (make-string "#(~A)" (klvm.test.str-join L " "))))

  (define fmt-list
    List -> (let L (map (function fmt) List)
              (make-string "(~A)" (klvm.test.str-join L " "))))

  (define fmt
    true -> "#t"
    false -> "#f"
    [] -> "()"
    [X | Xs] -> (fmt-list [X | Xs]) 
    X -> (fmt-vec X) where (vector? X)
    X -> (str X))

  (define fmt-def
    X R -> (make-string "~A => ~A" (fmt X) (fmt R)))

  (define defs
    [] Acc -> (let S (klvm.test.str-join (reverse Acc) "c#10; ")
                (make-string "(~A)~%" S))
    [(@p X R) | Ds] Acc -> (defs Ds [(fmt-def X R) | Acc]))

  (define pp-code
    [] S Sep -> S
    [X | Y] S Sep -> (let S (make-string "~A~A~R" S Sep X)
                       (pp-code Y S (make-string "~%    "))))

  (define pp-label
    [N | Code] S Sep -> (let S (make-string "~A~A(~A~%" S Sep N)
                             S (pp-code Code S "    ")
                          (cn S ")")))

  (define pp-labels'
    [[N | X]] S Sep -> (pp-label [N | X] S Sep)
    [[N | X] | Labels] S Sep -> (let Sep' (make-string "~%   ")
                                     S (pp-label [N | X] S Sep)
                                  (pp-labels' Labels S Sep')))

  (define pp-labels
    X S -> (cn (pp-labels' X S "  (") ")"))

  (define str-s2'
    [] S -> (cn S "c#10;")

    [[Head Name Args Nregs Code] | Y] S ->
    (let S (make-string "~A(~A ~R ~R ~R~%" S Head Name Args Nregs)
         S (pp-labels Code S)
         S (make-string "~A)~%~%" S)
      (str-s2' Y S))
    where (element? Head [klvm.func klvm.toplevel klvm.closure])

    [X | Y] S -> (let A (value *maximum-print-sequence-size*)
                      . (set *maximum-print-sequence-size* -1)
                      S (make-string "~A~S~%" S X)
                      . (set *maximum-print-sequence-size* A)
                     (str-s2' Y S)))

  (define str-s2
    X -> (str-s2' X "")))
