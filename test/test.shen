(package klvm.test [deftest]
  (set files ["test-code.shen"])

  (define from
    S -> (split-cases S [] []))

  (define from-file
    File -> (from (read-file File)))

  (define data
    -> (from (value data)))

  (define load-files
    [] Acc -> (set data Acc)
    [F | Fs] Acc -> (load-files Fs (append Acc (read-file F))))

  (load-files (value files) [])

  (define split-cases
    [] Cases Code -> (@p (kl-from-shen (reverse Code)) (reverse Cases))

    [[deftest Test Ret] | Xs] Cases Code ->
    (let T' (map (function parse-arg) Test)
         R' (parse-arg Ret)
      (split-cases Xs [(@p T' R') | Cases] Code))

    [X | Xs] Cases Code -> (split-cases Xs Cases [X | Code]))

  (define parse-vector
    [vector 0] -> <>
    [@v X Xs] -> (@v (parse-arg X) (parse-vector Xs))
    X -> (error "Unexpected @v tail: ~S" X))

  (define parse-list
    [] -> []
    [cons X Xs] -> [(parse-arg X) | (parse-list Xs)])

  (define list-from-vec'
    E E _ Acc -> (reverse Acc)
    I E V Acc -> (list-from-vec' (+ I 1) E V [(<-vector V I) | Acc]))

  (define list-from-vec
    V -> (list-from-vec' 1 (+ (limit V) 1) V []))

  (define parse-arg
    [@v X Xs] -> (parse-vector [@v X Xs])
    [cons X Xs] -> (parse-list [cons X Xs])
    X -> X)

  (define str-join'
    [] S R -> R
    [X | L] S "" -> (str-join' L S (make-string "~A" X))
    [X | L] S R -> (str-join' L S (cn R (cn S (make-string "~A" X)))))

  (define str-join
    X S -> (str-join' X S ""))

  (define str-from-sexpr
    How X -> (let M (value *maximum-print-sequence-size*)
                  . (set *maximum-print-sequence-size* -1)
                  S (trap-error
                     (make-string How X)
                     (/. E (do (set *maximum-print-sequence-size* M)
                               (error (error-to-string E)))))
                  . (set *maximum-print-sequence-size* M)
               S))
               
 (define kl-from-shen
   X -> (let X (shen.walk (function macroexpand) X)
             X (if (shen.packaged? X)
                   (package-contents X)
                   X)
          (shen.elim-def (shen.proc-input+ X)))))
