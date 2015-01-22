(package klvm.bytecode []
  (define cut-package*
    "" W -> W
    (@s "." Cs) W -> (cut-package* Cs "")
    (@s C Cs) W -> (cut-package* Cs (@s W C)))

  (define cut-package
    S -> (intern (cut-package* (str S) "")))

  (define collect-enum-defs
    [] Val Acc -> (reverse Acc)
    [[C V] | Xs] Val Acc -> (collect-enum-defs Xs (+ V 1) [[define C -> V] | Acc])
    [C | Xs] Val Acc -> (collect-enum-defs Xs (+ Val 1) [[define C -> Val] | Acc])))

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

(read-from-string "(package klvm.bytecode [] (klvm.bytecode.def-backend-fn mk-code [] C))")

(read-from-string "(package klvm.bytecode [] (klvm.bytecode.def-backend-fn closure-> (X Nargs C Acc) C))")

