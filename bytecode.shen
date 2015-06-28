(package klvm.bytecode [klvm.s1.translate

                        klvm.bytecode.walk
                        klvm.dbg

                        klvm.s1.const?

                        klvm.native klvm.reg klvm.reg-> klvm.call
                        klvm.tailcall klvm.tailif klvm.if klvm.return
                        klvm.mk-closure klvm.push-error-handler
                        klvm.pop-error-handler klvm.lambda]

(defstruct context
  (func symbol)
  (type symbol)
  (frame-size number)
  (frame-size-extra number)
  (nargs number)
  (toplevel (list A))
  (const table)
  (jumps table)
  (backend backend))

(klvm.bytecode.def-backend backend
  (native (A --> klvm.context --> A) (X C) C)
  (mk-code (--> A) () C)
  (code-len (A --> number) (X) C)
  (code-append! (A --> A --> A) (X Y) C)
  (prep-code (context --> A --> A) (X) C)
  (funcall (unit --> number --> A --> unit --> context --> B --> B)
           (F Nargs Ret-reg Args C Acc) C)
  (tailcall (unit --> number --> unit --> context --> B --> B)
            (F Nargs Args C Acc) C)
  (load-reg (number --> number --> context --> A --> A) (To From C Acc) C)
  (load-lambda (number --> symbol --> context --> A --> A) (To From C Acc) C)
  (load-const (number --> B --> context --> A --> A) (To X C Acc) C)
  (jump (number --> context --> A --> A) (Where C Acc) C)
  (if-reg-expr (number --> number --> context --> A --> A) (Reg Else C Acc) C)
  (ret-reg (number --> context --> A --> A) (X C Acc) C)
  (ret-lambda (B --> context --> A --> A) (X C Acc) C)
  (ret-const (B --> context --> A --> A) (X C Acc) C)
  (push-error-handler (unit --> context --> A --> A) (X C Acc) C)
  (pop-error-handler (context --> A --> A) (C Acc) C)
  (emit-func (symbol --> symbol --> (list symbol) --> number --> number
                     --> context --> A --> A)
             (Type Name Args Frame-size Frame-size-extra Code C Acc)
             C))

(define ensure-const*
  X Type N [] List -> (@p N [[X N | Type] | List])
  X Type N [[X I | Type] | Ys] List -> (@p I List)
  X Type N [_ | Ys] List -> (ensure-const* X Type (+ N 1) Ys List))

(define ensure-const
  X Type List -> (ensure-const* X Type 0 List List))

(define const
  X Type C -> (let R (ensure-const X Type (context-const C))
                   . (context-const-> C (snd R))
                (fst R)))

(define reg->
  To [klvm.reg From] C Acc -> (load-reg To From C Acc)
  To [klvm.lambda L] C Acc -> (load-lambda To L C Acc)
  To X C Acc -> (load-const To X C Acc) where (klvm.s1.const? X))

(define prepare-args
  [] _ _ Acc -> Acc
  [[I | X] | Xs] C Off Acc -> (let Acc (reg-> (+ I Off) X C Acc)
                                (prepare-args Xs C Off Acc)))

(define arg->
  [klvm.reg From] C Acc -> (arg-reg From C Acc)
  [klvm.lambda L] C Acc -> (arg-lambda L C Acc)
  X C Acc -> (arg-const X C Acc))

(define tail-arg->
  To [klvm.reg From] C Acc -> (tail-arg-reg To From C Acc)
  To [klvm.lambda L] C Acc -> (tail-arg-lambda To L C Acc)
  To X C Acc -> (tail-arg-const To X C Acc))

(define put-call-args
  [] _ Acc -> Acc
  [[I | X] | Xs] I Acc -> (put-call-args Xs (+ I 1) (arg-> X Acc))
  [[I | X] | Xs] Off Acc -> (error "Non tailcall args is not sequential."))

(define put-tail-call-args
  [] _ Acc -> Acc
  [[I | X] | Xs] _ Acc -> (put-tail-call-args Xs (tail-arg-> I X Acc)))

(define walk-call
  F Nargs Ret-reg X C Acc -> (let Acc (closure-> F Nargs C Acc)
                                  Acc (put-call-args X C Acc)
                               (funcall F Ret-reg C Acc)))

(define walk-tailcall
  F Nargs X C Acc -> (let Acc (closure-tail-> F Nargs C Acc)
                          Acc (put-tail-call-args X C Acc)
                       (tailcall F C Acc)))

(define walk-call
  F Nargs Ret-reg Args C Acc -> (funcall F Nargs Ret-reg Args C Acc))

(define walk-tailcall
  F Nargs Args C Acc -> (tailcall F Nargs Args C Acc))

(define walk-return
  [klvm.reg Reg] C Acc -> (ret-reg Reg C Acc)
  [klvm.lambda X] C Acc -> (ret-lambda X C Acc)
  X C Acc -> (ret-const X C Acc) where (klvm.s1.const? X))

(define if-jump
  \\ TODO: optimize zero offset jumps
  Where true C Acc -> Acc
  Where false C Acc -> (jump Where C Acc))

(define then-code-len
  Code true C -> (code-len Code C)
  Code false C -> (+ (code-len Code C) 1))

(define walk-if
  \\ TODO: optimize NOP 'if' branches
  [klvm.reg R] Then Else Tail? C Acc ->
  (let Then-code (walk-x1 Then C (mk-code C))
       Else-code (walk-x1 Else C (mk-code C))
       Then-code-len (then-code-len Then-code Tail? C)
       Acc (if-reg-expr R Then-code-len C Acc)
       Acc (code-append! Acc Then-code C)
       Acc (if-jump (code-len Else-code C) Tail? C Acc)
     (code-append! Acc Else-code C)))

(define walk-do
  [X] C Acc -> (walk-x1 X C Acc)
  [X | Xs] C Acc -> (walk-do Xs C (walk-x1 X C Acc)))

(define walk-x1
  [do | X] C Acc -> (walk-do X C Acc)
  [klvm.tailif If Then Else] C Acc -> (walk-if If Then Else true C Acc)
  [klvm.if If Then Else] C Acc -> (walk-if If Then Else false C Acc)
  [klvm.call F Nargs Ret-reg X] C Acc -> (walk-call F Nargs Ret-reg X C Acc)
  [klvm.tailcall F Nargs X] C Acc -> (walk-tailcall F Nargs X C Acc)
  [klvm.reg-> R X] C Acc -> (reg-> R X C Acc)
  [klvm.return X] C Acc -> (walk-return X C Acc)
  [klvm.push-error-handler E] C Acc -> (push-error-handler E C Acc)
  [klvm.pop-error-handler] C Acc -> (pop-error-handler C Acc)
  [] _ Acc -> Acc
  X _ _ <- (do (output "klvm.bytecode.walk-x1: Unexpected L1 expression: ~S~%"
                       X)
               (fail))
  _ _ Acc -> Acc
  X _ _ -> (error "klvm.bytecode.walk-x1: Unexpected L1 expression: ~S~%" X))

(define walk-toplevel-expr
  [Type Name Args Frame-size Frame-size-extra Code] S+ B Acc ->
  (let Arity (length Args)
       Frame-size' (+ Frame-size S+)
       C (mk-context Name Type Frame-size' Frame-size-extra Arity Acc [] [] B)
       X (walk-x1 Code C (mk-code C))
       Acc' (context-toplevel C)
    (emit-func Type Name Args Frame-size' Frame-size-extra X C Acc'))
  X _ _ _ -> (error "klvm.bytecode.walk-toplevel-expr: unexpected expr ~S~%"
                    X))

(define walk-toplevel
  [] S B Acc -> ((backend-prep-code B) _ Acc)
  [X | Xs] S B Acc -> (walk-toplevel Xs S B (walk-toplevel-expr X S B Acc)))

(define klvm.bytecode.compile
  X S+ B -> (let Code ((backend-mk-code B))
                 S1 (klvm.s1.translate (backend-native B) X true)
              (walk-toplevel S1 S+ B Code)))
)

