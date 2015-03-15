(package klvm.bytecode [binary.bitwise-ior binary.bitwise-and
                        binary.arithmetic-shift | & >> << even? div divisible-by?]
  (define cut-package*
    "" W -> W
    (@s "." Cs) W -> (cut-package* Cs "")
    (@s C Cs) W -> (cut-package* Cs (@s W C)))

  (define cut-package
    S -> (intern (cut-package* (str S) "")))

  (define collect-enum-defs
    [] Val Acc -> (reverse Acc)
    [[C V] | Xs] Val Acc -> (collect-enum-defs Xs (+ V 1) [[define C -> V] | Acc])
    [C | Xs] Val Acc -> (collect-enum-defs Xs (+ Val 1) [[define C -> Val] | Acc]))

  (define unwind-expr
    _ [X] -> (xbin-func X)
    Op [X Y] -> (Op (xbin-func X) (xbin-func Y))
    Op [X Y | Xs] -> (Op (Op (xbin-func X) (xbin-func Y))
                         (unwind-expr Op Xs)))

  (define xbin-func
    X -> X where (number? X)
    [| X | Xs] -> (unwind-expr binary.bitwise-ior [X | Xs])
    [& X | Xs] -> (unwind-expr binary.bitwise-and [X | Xs])
    [>> X Y] -> (binary.arithmetic-shift (xbin-func X) (- 0 (xbin-func Y)))
    [<< X Y] -> (binary.arithmetic-shift (xbin-func X) (xbin-func Y)))

  (define bin-func'
    0 R _ -> R
    X R L -> (bin-func' (div X 10) R (* L 2)) where (divisible-by? X 10) 
    X R L -> (bin-func' (div X 10) (+ R L) (* L 2)))

  (define bin-func
    X -> (bin-func' X 0 1)))

(define klvm.bytecode.def-enum-func
  Defs -> [package null [] | (klvm.bytecode.collect-enum-defs Defs 0 [])])

(define klvm.bytecode.def-backend-fn-func
  Func Args Carg -> (let Head [define Func]
                         Args' (if (element? Carg Args)
                                   Args
                                   (append Args [Carg]))
                         N (concat backend- (klvm.bytecode.cut-package Func))
                         Code [[N [context-backend Carg]]
                               | Args]
                      (append Head Args' [-> Code])))

(defmacro klvm.bytecode.def-backend-macro
  [klvm.bytecode.def-backend-fn Func Args Carg] ->
  (klvm.bytecode.def-backend-fn-func Func Args Carg))

(defmacro klvm.bytecode.def-enum
  [klvm.bytecode.def-enum | Defs] -> (klvm.bytecode.def-enum-func Defs))

(defmacro klvm.bytecode.xbin
  [klvm.bytecode.bin.xbin X] -> (klvm.bytecode.xbin-func X)
  [klvm.bytecode.bin.bin X] -> (klvm.bytecode.bin-func X) where (number? X))

\*
(read-from-string "(package klvm.bytecode [] (klvm.bytecode.def-backend-fn mk-code [] C))")

(read-from-string "(package klvm.bytecode [] (klvm.bytecode.def-backend-fn closure-> (X Nargs C Acc) C))")
*\

