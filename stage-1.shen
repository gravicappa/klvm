(package klvm.s1 [deinline-expr.deinline klvm-dump regkl.walk
                  regkl.get-arg regkl.get-reg regkl.set-reg! regkl.closure
                  regkl.func regkl.toplevel regkl.freeze]
[])

(defstruct context
  (func symbol)
  (nregs number)
  (nargs number)
  (stack-size number)
  (toplevel s-expr)
  (native (A --> context --> A)))

(define warn X -> (output "Warning: ~A" X))
(define warn-type -> (warn "`type` expression is not supported yet"))

(define const?
  X -> true where (symbol? X)
  X -> true where (string? X)
  X -> true where (number? X)
  _ -> false)

(define func-reg X C -> (+ (context-nargs C) X))
(define func-arg X C -> (- (context-nargs C) (+ X 1)))

(define reg-from-arg
  [regkl.get-reg R | _] C -> (func-reg R C)
  [regkl.get-arg R | _] C -> (func-arg R C)
  X _ -> [X])

(define regs-from-args
  [] C Acc -> Acc
  [X | Xs] C Acc -> (regs-from-args Xs C [(reg-from-arg X C) | Acc]))

(define set-call-args 
  [] _ Acc -> Acc
  [A | As] I Acc -> (let X [klvm-reg A]
                      (set-call-args As (+ I 1) [[klvm-reg-> I X] | Acc]))
                    where (number? A)
  [[A] | As] I Acc -> (set-call-args As (+ I 1) [[klvm-reg-> I A] | Acc])
  [X | As] _ _ -> (error "Unexpected arg expr: ~S~%" X))

(define comp-call F Args Return-reg C ->
  (let R (regs-from-args Args C [])
       Nargs (length R)
       . (set-call-args R C)
    (put-opcode klvm.call C)))

(define closure Return-reg Args Nregs Init Code C ->
  (let Ninit (length Init)
       Nargs (+ Ninit (length Args))
       F (gensym klvm-lambda)
       TL (context-toplevel C)
       A (klvm-trans.closure-args (protect A) 0 Nargs [])
       . (context-toplevel-> C (comp-func regkl.closure F A Nregs Code TL))
    (if (= Return-reg [])
        (comp-tailcall F Init C)
        (comp-call F Init Return-reg C))))

(define comp-freeze
  Tgt-reg Nregs Init Code C -> (closure Tgt-reg [] Nregs Init Code C))

(define comp-expr3
  [type X Type] C -> (do (warn-type)
                         (comp-expr3 X C))
  [regkl.get-reg R] C -> (comp-reg (func-reg R C) C)
  [regkl.get-arg R] C -> (comp-arg (func-arg R C) C)
  X C -> (comp-const X C) where (const? X)
  X _ -> (fail))

(define walk-x2
  [type X Type] Return-reg C Acc -> (do (warn-type)
                                        (walk-x2 X Return-reg C Acc))
  
  [regkl.closure Args Nregs Init Body] Return-reg C ->
  (closure Return-reg Args Nregs Init Body C)

  [regkl.freeze Nregs Init Body] Return-reg C ->
  (comp-freeze Return-reg Nregs Init Body C)
  
  [F | Args] Return-reg C -> (comp-call F Args Return-reg C)
  X Return-reg C Acc -> (comp-load-const X C) where (const? X)
  X _ _ _ -> (error "Unexpected L2 Reg-KLambda expression ~S" X))

(define walk-if
  [[regkl.get-reg R] Then Else] Tail? C Acc ->
  (let T' (walk-x1 Then Tail? C [])
       E' (walk-x1 Else Tail? C [])
    [[if [regkl.get-reg R] T' E'] | Acc])

  X _ _ Acc -> (error "Broken Reg-KLambda ~S." X))

(define walk-do
  [X] Tail? C Acc -> (walk-x1 X Tail? C Acc)
  [X | Rest] Tail? C Acc -> (walk-do Rest Tail? (walk-x1 X false C)))

(define walk-trap
  X E Tail? C Acc -> (let X' (walk-x1 [

(define walk-x1 
  [do | X] Tail? C Acc -> (walk-do X Tail? C Acc)
  [if | X] Tail? C Acc -> (walk-if X Tail? C Acc)
  [regkl.get-reg R] true C Acc -> [[get-reg (func-reg R C)] | Acc]
  [regkl.get-reg _] false C Acc -> Acc
  [regkl.set-reg! R X2] false C Acc -> (walk-x2 X2 (func-reg R C) C Acc)
  [regkl.get-arg R] true C Acc -> [[get-reg (func-arg R C)] | Acc]
  [regkl.get-arg _] false C Acc -> Acc
  [regkl.closure _ _ _ _] false _ Acc ->  Acc
  [regkl.freeze _ _ _] false _ Acc -> Acc
  [trap-error X E] Tail? C Acc -> (walk-trap X E Tail? C Acc)

  [regkl.closure Args Nregs Init Body] true C Acc ->
  (walk-closure [] Args Nregs Init Body C Acc)

  [regkl.freeze Nregs Init Body] true C Acc ->
  (walk-freeze [] Nregs Init Body C Acc)

  [F | Args] true C Acc -> (walk-tailcall F Args C Acc)
  [F | Args] false C Acc -> (walk-call F Args [] C Acc)
  _ false _ Acc -> Acc
  X true C Acc -> (comp-return-const X C Acc)
  X _ _ _ -> (error "Unexpected L1 Reg-KLambda expression ~S" X))

(define walk-body
  [] C Acc -> (reverse Acc)
  [X | Y] C Acc -> (walk-body Y C (walk-x1 X C)))

(define func-code
  regkl.func -> func
  regkl.toplevel -> toplevel
  regkl.closure -> closure)

(define walk-func Type Name Args Nregs Body Fn Toplevel -> 
  (let Nargs (length Args)
       C (mk-context Name Nregs Nargs (+ Nregs Nargs 2) Toplevel Fn)
       Body' (walk-body Body C)
    [ | (context-toplevel C)]))

(define walk-1
  [Type Name Args Nregs Body] Fn Toplevel -> 
  (walk-func Type Name Args Nregs Body Fn Toplevel)
  where (element? Type [regkl.func regkl.toplevel regkl.closure])
  [X | Y] _ _ -> (error "Unexpected toplevel expression ~S~%" [X | Y])
  X _ _ -> Toplevel)

(define walk-toplevel'
  [] _ Acc -> (reverse Acc)
  [X | Y] Fn Acc -> (walk-toplevel' Y (walk-1 X Fn Acc)))

(define walk
  Fn X -> (walk-toplevel' X Fn []))
