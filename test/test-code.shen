(defun regkl.trap-error (X E)
  (do (klvm.push-error-handler E)
    (let R (X)
      (do (klvm.pop-error-handler)
          R))))

(deftest (+ 7 8) 15)
(deftest (- 7 8) -1)

(define test-if
  X -> (if X yes no))

(deftest (test-if true) yes)
(deftest (test-if false) no)

(define test-ret-elim
  X -> (let A (if X yes no)
         X))

(deftest (test-ret-elim true) true)
(deftest (test-ret-elim false) false)
(deftest (test-ret-elim 3) 3)

(define list-len-aux
  [] Acc -> Acc
  [X | Xs] Acc -> (list-len-aux Xs (+ Acc 1)))

(define list-len
  List -> (list-len-aux List 0))

(deftest (list-len []) 0)
(deftest (list-len [1 2 3 4 a]) 5)

(define test-partial
  -> ((+ 2) 7))

(deftest (test-partial) 9)

(define adder
  Add -> (/. X (+ Add X)))

(define part1
  X -> (+ X))

(define part2
  X -> (let A (+ X)
         A))

(define test-partial-2
  -> (part2 10 20))

(deftest (test-partial-2) 30)

(define test-partial-3
  -> (part1 10 20))

(deftest (test-partial-3) 30)

(define test-closure
  -> (let X (adder 6)
       (X 8)))

(deftest (test-closure) 14)

(define test-tail-call
  -> (let X (adder 6)
          Y (X 8)
       (- Y 14)))

(deftest (test-tail-call) 0)

(define reversex-aux
  [] Acc -> Acc
  [X | Y] Acc -> (reversex-aux Y [X | Acc]))

(define reversex
  List -> (reversex-aux List []))

(deftest (reversex []) [])
(deftest (reversex [1 2 3 4 5]) [5 4 3 2 1])

(define mapx-aux
  _ [] Acc -> (reversex-aux Acc [])
  Fn [X | List] Acc -> (mapx-aux Fn List [(Fn X) | Acc]))

(define mapx
  Fn List -> (mapx-aux Fn List []))

(define incr-list
  List -> (mapx (+ 1) List))

(deftest (incr-list [-1 2 -3 4 -5 6]) [0 3 -2 5 -4 7])

(define incr-list-aux
  List -> (mapx (/. X (+ 1 X)) List))

(deftest (incr-list-aux [-1 2 -3 4 -5 6]) [0 3 -2 5 -4 7])

(define test-trap-error
  -> (trap-error (- 1 1) (/. E 10)))

(deftest (test-trap-error) 0)

(define test-trap-error-2
  -> (trap-error (/ 10 0) (/. E 0)))

(deftest (test-trap-error-2) 0)

(define test-trap-error-3
  -> (let X (+ 17 10)
          X (* X 2)
          X (trap-error (/ X 0) (/. E (+ X 1)))
       (- X 55)))

(deftest (test-trap-error-3) 0)

(define test-trap-error-4
  -> (let X (+ 17 10)
          X (* X 2)
          X (trap-error (undefined-func X 0) (/. E (+ X 1)))
       (- X 55)))

(deftest (test-trap-error-4) 0)

(define test-call
  X -> (+ 7 (+ 7 X)))

(deftest (test-call 9) 23)

(define test-map
  List -> (mapx (/. X (> X 0)) List)) 

(deftest (test-map []) [])
(deftest (test-map [-1 2 -3 4 -5 6]) [false true false true false true])

(define test-map-aux
  List -> (mapx-aux (/. X (> X 0)) List []))

(define test-freeze
  -> (let X 5
          F (freeze (* X 5))
          X 6
       (thaw F)))

(deftest (test-freeze) 25)

(define guard-aux
  [] Acc -> (reversex Acc)
  [X | Y] Acc -> (guard-aux Y [[str X] | Acc]) where (string? X)
  [X | Y] Acc -> (guard-aux Y [[num X] | Acc]) where (number? X)
  [X | Y] Acc -> (guard-aux Y [[sym X] | Acc]) where (symbol? X))

(define test-guard
  -> (guard-aux [1 two "three" "four" 5 six] []))

(deftest (test-guard) [[num 1] [sym two] [str "three"] [str "four"]
                       [num 5] [sym six]])

(define test-gensym -> (gensym (protect XYZ)))

(define appendx-aux
  [] [] Acc -> (reversex Acc)
  [] [Y | Ys] Acc -> (appendx-aux [] Ys [Y | Acc])
  [X | Xs] Y Acc -> (appendx-aux Xs Y [X | Acc]))

(define appendx
  X Y -> (appendx-aux X Y []))

(deftest (appendx [a b c] [1 2 3]) [a b c 1 2 3])

(define mkcurry
  F 0 -> [freeze F]
  F -1 -> F
  F N -> (mkcurry-aux F N []))

(define mkcurry-aux
  F 0 Vars -> [F | Vars]
  F N Vars -> (let X (gensym (protect V))
                [/. X (mkcurry-aux F (- N 1) (appendx Vars [X]))]))

(define test-mkcurry
  -> (let X (mkcurry func 5)
       X))

(define test-freeze-2-aux
  A B -> (let X 8
              Y (+ A B)
           (freeze (+ A (* X Y)))))

(define test-freeze-2
  -> (let F (test-freeze-2-aux 8 1)
       (thaw F)))

(deftest (test-freeze-2) 80)

(define test-do
  -> (let V (vector 3)
        (do (vector-> V 1 3)
            (vector-> V 2 (+ (<-vector V 1) 2))
            (vector-> V 3 (* (<-vector V 2) 3))
            V)))

(deftest (test-do) (@v 3 3 5 15 <>))

(define test-let-1
  X -> (let V1 (let V (vector 4)
                 (if X
                     (do (vector-> V 1 3)
                         (vector-> V 2 (+ (<-vector V 1) 2))
                         (vector-> V 3 (* (<-vector V 2) 3))
                         V)
                     (do (vector-> V 1 0)
                         (vector-> V 2 (+ (<-vector V 1) 1))
                         (vector-> V 3 (+ (<-vector V 2) 1))
                         V)))
         (do (vector-> V1 4 one)
             V1)))

(deftest (test-let-1 true) (@v 4 3 5 15 one <>))
(deftest (test-let-1 false) (@v 4 0 1 2 one <>))

(define list-5
  A B C D E -> [A B C D E])

(define test-closure-2
  -> (let X (list-5 1 2)
          Y (X 3 4)
       (Y 5)))

(deftest (test-closure-2) [1 2 3 4 5])

(define list-7
  A B C D E F G -> [A B C D E F G])

(define call-aux
  F [] A B -> (F A B)
  F [X | Y] A B -> (call-aux [F X] Y A B))

(define test-closure-3
  -> (call-aux (function list-7) [1 2 3 4 5] a b))

(deftest (test-closure-3) [1 2 3 4 5 a b])

(define test-closure-4
  -> (let X 5
          F (/. A (+ A X))
          G (/. F X (F X))
       (G F 12)))

(deftest (test-closure-4) 17)

(define test-closure-5
  -> (let X 5
          F (/. A (- A X))
        (F 5)))

(deftest (test-closure-5) 0)

(define test-and
  X Y -> (and X Y))

(deftest (test-and false false) false)
(deftest (test-and false true) false)
(deftest (test-and true false) false)
(deftest (test-and true true) true)

(define test-or
  X Y -> (or X Y))

(deftest (test-or false false) false)
(deftest (test-or false true) true)
(deftest (test-or true false) true)
(deftest (test-or true true) true)

(define test-and-2
  X Y -> ((and X) Y))

(deftest (test-and-2 false false) false)
(deftest (test-and-2 false true) false)
(deftest (test-and-2 true false) false)
(deftest (test-and-2 true true) true)

(define test-or-2
  X Y -> ((or X) Y))

(deftest (test-or-2 false false) false)
(deftest (test-or-2 false true) true)
(deftest (test-or-2 true false) true)
(deftest (test-or-2 true true) true)
