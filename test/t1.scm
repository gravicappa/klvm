
(define t1.defs
  '((+ 7 8) => 15
    (- 7 8) => -1
    (klvm-test.test-call 9) => 23
    (klvm-test.test-tail-call) => 0
    (klvm-test.test-if #t) => klvm-test.yes
    (klvm-test.test-if #f) => klvm-test.no
    (klvm-test.test-ret-elim #t) => #t
    (klvm-test.test-ret-elim #f) => #f
    (klvm-test.test-ret-elim 3) => 3
    (klvm-test.list-len ()) => 0
    (klvm-test.list-len (1 2 3 4 a)) => 5
    (klvm-test.reversex ()) => ()
    (klvm-test.reversex (1 2 3 4 5)) => (5 4 3 2 1)
    (klvm-test.test-partial) => 9
    (klvm-test.test-partial-2) => 30
    (klvm-test.test-partial-3) => 30
    (klvm-test.test-closure) => 14
    (klvm-test.test-closure-2) => (1 2 3 4 5)
    (klvm-test.test-closure-3) => (1 2 3 4 5 klvm-test.a klvm-test.b)
    (klvm-test.test-freeze) => 25
    (klvm-test.test-closure-4) => 17
    (klvm-test.test-closure-5) => 0
    (klvm-test.test-map ()) => ()
    (klvm-test.test-map (-1 2 -3 4 -5 6)) => (#f #t #f #t #f #t)
    (klvm-test.incr-list (-1 2 -3 4 -5 6)) => (0 3 -2 5 -4 7)
    (klvm-test.incr-list-aux (-1 2 -3 4 -5 6)) => (0 3 -2 5 -4 7)
    (klvm-test.test-guard) => ((klvm-test.num 1)
                               (klvm-test.sym klvm-test.two)
                               (str "three")
                               (str "four")
                               (klvm-test.num 5)
                               (klvm-test.sym klvm-test.six))
    (klvm-test.test-do) => #(3 3 5 15)
    (klvm-test.test-and #t #t) => #t
    (klvm-test.test-and #t #f) => #f
    (klvm-test.test-and #f #t) => #f
    (klvm-test.test-and #f #f) => #f
    (klvm-test.test-and-2 #t #t) => #t
    (klvm-test.test-and-2 #t #f) => #f
    (klvm-test.test-and-2 #f #t) => #f
    (klvm-test.test-and-2 #f #f) => #f
    (klvm-test.test-or #t #t) => #t
    (klvm-test.test-or #f #t) => #t
    (klvm-test.test-or #t #f) => #t
    (klvm-test.test-or #f #f) => #f
    (klvm-test.test-or-2 #t #t) => #t
    (klvm-test.test-or-2 #f #t) => #t
    (klvm-test.test-or-2 #t #f) => #t
    (klvm-test.test-or-2 #f #f) => #f
    (klvm-test.appendx (a b c) (2 3 4)) => (a b c 2 3 4)
    (klvm-test.test-let-1 #t) => #(4 3 5 15 klvm-test.one)
    (klvm-test.test-let-1 #f) => #(4 0 1 2 klvm-test.one)
    (klvm-test.test-freeze-2) => 80
    (klvm-test.test-trap-error) => 0
    (klvm-test.test-trap-error-2) => 0
    (klvm-test.test-trap-error-3) => 0
    (klvm-test.test-trap-error-4) => 0))
