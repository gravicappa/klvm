(package klvm.bytecode [klvm.s1.translate

                        klvm.bytecode.walk
                        klvm.dbg
                        klvm.kl-from-shen

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
  (code (list A))
  (global-const (list A))
  (local-const (list A))
  (jumps table)
  (backend backend))

(define mk-context'
  Backend -> (let Code ((backend-mk-code Backend))
               (mk-context _ _ 0 0 0 Code [] [] _ Backend)))

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

(define ensure-const
  X Type N [] Add -> (Add X Type N)
  X Type _ [[X Type I | _] | Entries] _ -> I
  X Type N [_ | Entries] Add -> (ensure-const X Type (+ N 1) Entries Add))

(define global-const-add
  C X Type I -> (let Entries (context-global-const C)
                     . (context-global-const-> C [[X Type I] | Entries])
                  I))

(define local-const-add
  C X Type I -> (let Global (ensure-global-const X Type C)
                     Entries (context-local-const C)
                     . (context-local-const-> C [[X Type I | Global] | Entries])
                  I))

(define ensure-global-const
  X Type C -> (let Entries (context-global-const C)
                (ensure-const X Type 0 Entries (global-const-add C))))

(define const
  X Type C -> (let Entries (context-local-const C)
                (ensure-const X Type 0 Entries (local-const-add C))))

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
  [[I | X] | Xs] Off Acc -> (error "~A: Non tailcall args is not sequential."
                                   put-call-args))

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
  X _ _ <- (do (output "~A: Unexpected L1 expression: ~S~%" walk-x1 X)
               (fail))
  _ _ Acc -> Acc
  X _ _ -> (error "~A: Unexpected L1 expression: ~S~%" walk-x1 X))

(define walk-toplevel-expr
  [Type Name Args Frame-size Frame-size-extra Code] S+ B C ->
  (let Arity (length Args)
       Frame-size' (+ Frame-size S+)
       . (context-func-> C Name)
       . (context-type-> C Type)
       . (context-frame-size-> C Frame-size')
       . (context-frame-size-extra-> C Frame-size-extra)
       . (context-nargs-> C Arity)
       . (context-local-const-> C [])
       X (walk-x1 Code C (mk-code C))
       A (context-code C)
       A (emit-func Type Name Args Frame-size' Frame-size-extra X C A)
       . (context-code-> C A)
    _)
  X _ _ _ -> (error "~A: unexpected expr ~S~%" walk-toplevel-expr X))

(define walk-toplevel
  [] S B C -> ((backend-prep-code B) C (context-code C))
  [X | Xs] S B C -> (do (walk-toplevel-expr X S B C)
                        (walk-toplevel Xs S B C)))

(define klvm.bytecode.compile
  X S+ B -> (let C (mk-context' B)
                 S1 (klvm.s1.translate (backend-native B) X true)
                 . (output "klvm.bytecode.compile produced S1~%")
              (walk-toplevel S1 S+ B C)))

(define kl-from-files'
  [] Acc -> (reverse Acc)
  [F | Fs] Acc -> (let Kl (klvm.kl-from-shen (read-file F))
                    (kl-from-files' Fs (append (reverse Kl) Acc))))

(define kl-from-files
  Files -> (kl-from-files' Files []))
)

