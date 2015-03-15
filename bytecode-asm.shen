(package klvm.bytecode.asm [klvm.bytecode.walk klvm.bytecode.mk-backend
                            klvm.bytecode-asm-from-kl
                            klvm.bytecode.asm.print
                            klvm.bytecode.cut-package
                            klvm.bytecode.const
                            klvm.bytecode.context-const
                            klvm.bytecode.context-frame-size

                            klvm.lambda
                            klvm.reg
                            
                            klvm.load-reg->
                            klvm.load-lambda->
                            klvm.load-const->
                            klvm.jump
                            klvm.closure-lambda->
                            klvm.closure-reg->
                            klvm.closure-fn->
                            klvm.closure-tail-lambda->
                            klvm.closure-tail-reg->
                            klvm.closure-tail-fn->
                            klvm.drop-ret
                            klvm.load-ret->
                            klvm.call
                            klvm.tail-call
                            klvm.jump-unless
                            klvm.ret-reg
                            klvm.ret-lambda
                            klvm.ret-const
                            klvm.push-error-handler
                            klvm.pop-error-handler
                            ]

(define const-type
  [] -> nil
  X -> bool where (boolean? X)
  X -> str where (string? X)
  X -> sym where (symbol? X)
  X -> num where (number? X))

(define const
  X C -> (klvm.bytecode.const X (const-type X) C))

(define mk-code
  -> [])

(define code-len
  X -> (length X))

(define code-append
  X Y -> (append Y X))

(define load-reg
  To From _ Acc -> [[klvm.load-reg-> To From] | Acc])

(define load-lambda
  To X C Acc -> (let X' (klvm.bytecode.const X lambda C)
                  [[klvm.load-lambda-> To X'] | Acc]))

(define load-const
  To X C Acc -> [[klvm.load-const-> To (const X C)] | Acc])

(define jump
  Where _ Acc -> [[klvm.jump Where] | Acc])

(define closure->
  [klvm.lambda L] Nargs C Acc -> (let X (klvm.bytecode.const L lambda C)
                                   [[klvm.closure-lambda-> X Nargs] | Acc])
  [klvm.reg R] Nargs _ Acc -> [[klvm.closure-reg-> R Nargs] | Acc]
  X Nargs C Acc -> (let X' (klvm.bytecode.const X func C)
                     [[klvm.closure-fn-> X' Nargs] | Acc])
                   where (symbol? X))

(define closure-tail->
  [klvm.lambda L] Nargs C Acc -> (let X (klvm.bytecode.const L lambda C)
                                      Y [klvm.closure-tail-lambda-> X Nargs]
                                   [Y | Acc])
  [klvm.reg R] Nargs _ Acc -> [[klvm.closure-tail-reg-> R Nargs] | Acc]
  X Nargs C Acc -> (let X' (klvm.bytecode.const X func C)
                     [[klvm.closure-tail-fn-> X' Nargs] | Acc])
                   where (symbol? X))

(define arg->
  To [klvm.reg From] C Acc -> [[klvm.load-reg-> To From] | Acc]
  To [klvm.lambda L] C Acc -> (let X (klvm.bytecode.const L lambda C)
                                [[klvm.load-lambda-> To X] | Acc])
  To X C Acc -> [[klvm.load-const-> To (const X C)] | Acc])

(define call-args
  [] _ _ C Acc -> Acc
  [[I | X] | Xs] I J C Acc -> (call-args
                               Xs (+ I 1) (+ J 1) C (arg-> J X C Acc))
  [[I | X] | Xs] _ S C Acc -> (error "Non tailcall args is not sequential."))

(define tail-call-args
  [] _ Acc -> Acc
  [[I | X] | Xs] C Acc -> (tail-call-args Xs C (arg-> I X C Acc)))

(define load-ret
  [] Acc -> [[klvm.drop-ret] | Acc]
  Ret-reg Acc -> [[klvm.load-ret-> Ret-reg] | Acc])

(define funcall
  F Nargs Ret-reg Args C Acc -> (let Acc (closure-> F Nargs C Acc)
                                     S (klvm.bytecode.context-frame-size C)
                                     Acc (call-args Args 0 S C Acc)
                                     Acc [[klvm.call] | Acc]
                                  (load-ret Ret-reg Acc)))

(define tailcall
  F Nargs Args C Acc -> (let Acc (closure-tail-> F Nargs C Acc)
                             Acc (tail-call-args Args C Acc)
                          [[klvm.tail-call] | Acc]))

(define if-reg-expr
  Reg Else-Offset _ Acc -> [[klvm.jump-unless Reg Else-Offset] | Acc])

(define ret-reg
  Reg _ Acc -> [[klvm.ret-reg Reg] | Acc])

(define ret-lambda
  Fn C Acc -> (let X (klvm.bytecode.const Fn func C)
                [[klvm.ret-lambda X] | Acc]))

(define ret-const
  X C Acc -> [[klvm.ret-const (const X C)] | Acc])

(define push-error-handler
  [klvm.reg Reg] _ Acc -> [[klvm.push-error-handler Reg] | Acc])

(define pop-error-handler
  _ Acc -> [[klvm.pop-error-handler] | Acc])

(define emit-func
  Type Name Args Frame-size Frame-size-extra Code C Acc ->
  (let Const (reverse (klvm.bytecode.context-const C))
       Code' (reverse Code)
    [[Type Name Args Frame-size Frame-size-extra Const Code'] | Acc]))

(define prep-code
  _ X -> (reverse X))

(set backend (klvm.bytecode.mk-backend [] mk-code code-len code-append
                                       prep-code funcall tailcall load-reg
                                       load-lambda load-const jump if-reg-expr
                                       ret-reg ret-lambda ret-const
                                       push-error-handler pop-error-handler
                                       emit-func))

(define walk
  X S+ -> (klvm.bytecode.walk X S+ (value backend)))

(define klvm.bytecode-asm-from-kl
  X S+ -> (walk X S+))

(define str-join*
  [] _ _ Acc -> Acc
  [X] _ F Acc -> (make-string (cn "~A" F) Acc X)
  [X | Xs] Sep F Acc -> (let Fmt (cn "~A" (cn F "~A"))
                          (str-join* Xs Sep F (make-string Fmt Acc X Sep))))

(define str-join
  List Sep F -> (str-join* List Sep F ""))

(define esc-str*
  "" Ret -> Ret
  (@s C Cs) Ret -> (esc-str* Cs (cn Ret "\c#34;")) where (= C "c#34;")
  (@s C Cs) Ret -> (esc-str* Cs (cn Ret "\n")) where (= C "c#10;")
  (@s C Cs) Ret -> (esc-str* Cs (cn Ret "\r")) where (= C "c#13;")
  (@s C Cs) Ret -> (esc-str* Cs (cn Ret C)))

(define esc-const
  X -> (esc-str* X "") where (string? X)
  X -> X)

(define comment
  "" -> ""
  X -> (cn " ; " X))

(define print-op*
  Op Args Const-idc Const Stream ->
  (let Op' (klvm.bytecode.cut-package Op)
       C (map (/. I (head (<-vector Const I))) (map (+ 1) Const-idc))
       . (pr (str-join [Op' | Args] " " "~S") Stream)
       . (pr (esc-const (comment (str-join C " " "~S"))) Stream)
       . (pr (n->string 10) Stream)
    true))

(define print-op
  Op [A B] Const Stream -> (print-op* Op [A B] [B] Const Stream)
                           where (element? Op [klvm.load-const->
                                               klvm.load-lambda->])

  Op [A] Const Stream -> (print-op* ret-lambda [A] [A] Const Stream)
                         where (element? Op [klvm.ret-const klvm.ret-lambda])

  Op [A B] Const Stream -> (print-op* Op [A B] [A] Const Stream)
                           where (element? Op [klvm.closure-lambda->
                                               klvm.closure-fn->
                                               klvm.closure-tail-lambda->
                                               klvm.closure-tail-fn->])

  Op Args Const Stream -> (print-op* Op Args [] Const Stream))

(define print-func-code
  [] _ _ -> true
  [[Op | Args] | Ops] Const Stream -> (do (print-op Op Args Const Stream)
                                          (print-func-code Ops Const Stream)))

(define print-const-table
  [] Stream -> true
  [[X _ | Type] | Cs] Stream -> (let T' (klvm.bytecode.cut-package Type)
                                     X' (esc-const X)
                                     . (pr (make-string "const ~A ~S~%" T' X')
                                           Stream)
                                  (print-const-table Cs Stream)))

(define vec<-list*
  [] N N V -> V
  [X | Xs] I N V -> (vec<-list* Xs (+ I 1) N (vector-> V (+ I 1) X)))

(define vec<-list
  L -> (let V (vector (length L))
         (vec<-list* L 0 (limit V) V)))

(define print-func
  [Type Name Args Size Size+ Const Code] Stream ->
  (let Arity (length Args)
       . (pr (make-string "~%~A ~A ~A ~A ~A~%" Type Name Arity Size Size+)
             Stream)
       . (print-const-table Const Stream)
       V (vec<-list Const)
    (print-func-code Code V Stream)))

(define klvm.bytecode.asm.print
  [] Stream -> true
  [Asm | Asms] Stream -> (do (print-func Asm Stream)
                             (klvm.bytecode.asm.print Asms Stream)))

)
