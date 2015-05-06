(package klvm.s1 [denest.translate walk skip
                  regkl.translate regkl.arg regkl.reg regkl.reg->
                  regkl.closure regkl.func regkl.toplevel regkl.freeze
                  regkl.trap-error

                  klvm.reg klvm.reg-> klvm.call klvm.tailcall klvm.tailif
                  klvm.if klvm.return klvm.mk-closure klvm.push-error-handler
                  klvm.pop-error-handler klvm.lambda]

(defstruct context
  (func symbol)
  (nregs number)
  (arity number)
  (frame-size number)
  (frame-size-extra number)
  (toplevel s-expr)
  (quote (A --> context --> A)))

(define warning X -> (output "Warning: ~A" X))
(define warn-type -> (warning "`type` expression is not supported yet"))

(define const?
  [] -> true
  X -> true where (symbol? X)
  X -> true where (string? X)
  X -> true where (number? X)
  X -> true where (boolean? X)
  _ -> false)

(define func-reg X C -> (+ (context-arity C) X))
(define func-arg X C -> (- (context-arity C) (+ X 1)))

(define upd-context-frame-size-extra
  N C -> (context-frame-size-extra-> C N)
         where (> N (context-frame-size-extra C))
  _ C -> C)

(define set-call-args 
  [] _ Acc -> Acc
  [A | As] I Acc -> (set-call-args As (- I 1) [[I | A] | Acc]))

(define walk-call
  F Args Return-reg C Acc -> (let Nargs (length Args)
                                  . (upd-context-frame-size-extra Nargs C)
                                  Args' (set-call-args Args (- Nargs 1) [])
                               [[klvm.call F Nargs Return-reg Args'] | Acc]))

(package tail [klvm.reg]
  (defstruct arg
    (i number)
    (value unit)
    (deps (list number)))

  (define tree-find-all'
    [] _ Acc -> Acc
    X F Acc -> [X | Acc] where (F X)
    [X | Y] F Acc -> (tree-find-all' Y F [X | Acc]) where (F X)
    [[X | Xs] | Y] F Acc -> (let Acc (tree-find-all' [X | Xs] F Acc)
                              (tree-find-all' Y F Acc))
    [X | Y] F Acc -> (tree-find-all' Y F Acc)
    _ _ Acc -> Acc)

  (define tree-find-all''
    [] _ Acc -> Acc
    [X | Y] F Acc -> (tree-find-all'' Y F [X | Acc]) where (F X)
    [[X | Xs] | Y] F Acc -> (let Acc (tree-find-all' [X | Xs] F Acc)
                              (tree-find-all'' Y F Acc))
    [X | Y] F Acc -> (tree-find-all'' Y F Acc)
    _ _ Acc -> Acc)

  (define tree-find-all'
    X F Acc -> [X | Acc] where (F X)
    X F Acc -> (tree-find-all'' X F Acc))

  (define tree-find-all
    F Tree -> (reverse (tree-find-all' Tree F [])))

  (define reg?
    [klvm.reg _] -> true
    X -> false)

  (define reg-num
    [klvm.reg X] -> X)

  (define <-vec
    V I -> (<-vector V (+ I 1)))

  (define vec->
    V I X -> (vector-> V (+ I 1) X))

  (define find-arg-min-ref
    [] _ Mx _ -> Mx
    [X | Xs] Ref Mx Mr -> (let F (find-arg-min-ref Xs Ref)
                               R (<-vec Ref (arg-i X))
                            (if (or (< Mr 0) (< R Mr))
                                (F X R)
                                (F Mx Mr))))

  (define dec-ref-count
    I Ref -> (let X (<-vec Ref I)
               (if (> X 0)
                   (vec-> Ref I (- X 1))
                   Ref)))

  (define find-tmp-place
    I Ref -> I where (= (<-vec Ref I) 0)
    I Ref -> (find-tmp-place (+ I 1) Ref))

  (define remove-arg
    [] _ Acc -> Acc
    [X | Xs] A Acc -> (remove-arg Xs A Acc) where (= (arg-i X) (arg-i A))
    [X | Xs] A Acc -> (remove-arg Xs A [X | Acc]))

  (define remap-and-deref
    [klvm.reg R] Self Ref Map -> (let R' (<-vec Map R)
                                      . (if (= R Self)
                                            _
                                            (dec-ref-count R' Ref))
                                   [klvm.reg R'])
    [X | Xs] Self Ref Map -> (let F (/. X (remap-and-deref X Self Ref Map))
                               (map F [X | Xs]))
    X _ _ _ -> X)

  (define mov-arg'
    I X Acc -> [[I | X] | Acc])

  (define mov-arg
    I X Ref Map Acc -> (mov-arg' I (remap-and-deref X I Ref Map) Acc))

  (define put-arg-1
    Args N Ref X Map Acc -> (let A (mov-arg (arg-i X) (arg-value X) Ref Map Acc)
                              (put-iter Args N Ref Map A)))

  (define put-arg-N
    Args N Ref X Map Acc -> (let 
                                 T (find-tmp-place N Ref)
                                 \\. (output "X: ~S~%" X)
                                 \\. (output "tmp: ~S~%" T)
                                 \\. (output "ref 1: ~S~%" Ref)
                                 A (mov-arg' T [klvm.reg (arg-i X)] Acc)
                                 \\. (output "ref 2: ~S~%" Ref)
                                 A (mov-arg (arg-i X) (arg-value X) Ref Map A)
                                 \\. (output "Acc: ~S~%" A)
                                 \\. (output "ref 3: ~S~%" Ref)
                                 . (vec-> Ref T (<-vec Ref (arg-i X)))
                                 . (vec-> Ref (arg-i X) 0)
                                 . (vec-> Map (arg-i X) T)
                              (put-iter Args N Ref Map A)))

  (define put-iter
    [] _ _ _ Acc -> (reverse Acc)
    Args N Ref Map Acc -> (let A (find-arg-min-ref Args Ref [] -1)
                               \\. (output "~%## put-iter~%")
                               \\. (output "min ref: ~S~%" A)
                               Args (remove-arg Args A [])
                               \\. (output "args': ~S~%" Args)
                               R (<-vec Ref (arg-i A))
                               \\. (output "R: ~S~%" R)
                            (if (= R 0)
                                (put-arg-1 Args N Ref A Map Acc)
                                (put-arg-N Args N Ref A Map Acc))))

  (define init-args
    [] _ Acc -> (reverse Acc)
    [X | Args] I Acc -> (let Deps (tree-find-all (function reg?) X)
                             A (mk-arg I X (map (function reg-num) Deps))
                          (init-args Args (+ I 1) [A | Acc])))

  (define init-map
    N N V -> V
    I N V -> (do (vector-> V I (- I 1))
                 (init-map (+ I 1) N V)))

  (define init-arg-ref
    [] I V -> V
    [R | Rs] R V -> (init-arg-ref Rs R V)
    [R | Rs] I V -> (do (vec-> V R (+ (<-vec V R) 1))
                        (init-arg-ref Rs I V)))

  (define init-ref-vec
    N N V -> V
    I N V -> (do (vector-> V I 0)
                 (init-ref-vec (+ I 1) N V)))

  (define init-ref
    [] V -> V
    [A | As] V -> (do (init-arg-ref (arg-deps A) (arg-i A) V)
                      (init-ref As V)))

  (define vec-size'
    [] S -> S
    [D | Ds] S -> (vec-size' Ds (if (> D S) D S)))

  (define vec-size
    [] S -> S
    [A | As] S -> (vec-size As (vec-size' (arg-deps A) S)))

  (define put-args
    Args -> (let N (length Args)
                 A (init-args Args 0 [])
                 Vsize (+ (vec-size A N) 1 N)
                 Ref (init-ref A (init-ref-vec 1 (+ Vsize 1) (vector Vsize)))
                 Map (init-map 1 (+ Vsize 1) (vector Vsize))
              (put-iter A N Ref Map [])))

  (define max-arg-num
    [] M -> M
    [[Reg | X] | Code] M -> (max-arg-num Code Reg) where (< M Reg)
    [[Reg | X] | Code] M -> (max-arg-num Code M)
    X _ -> (error "~A: unrecognized data ~S~%" max-arg X)))

(define walk-tailcall
  F Args C Acc ->
  (let Nargs (length Args)
       Code (tail.put-args (reverse Args))
       . (upd-context-frame-size-extra (+ (tail.max-arg-num Code Nargs) 1) C)
    [[klvm.tailcall F Nargs Code] | Acc]))

(define closure-args
  S N N Acc -> (reverse Acc)
  S I N Acc -> (closure-args S (+ I 1) N [(concat S I) | Acc]))

(define mk-closure
  F [] Init [] C Acc -> (walk-tailcall klvm.mk-closure [F | Init] C Acc)

  F [] Init Return-reg C Acc ->
  (walk-call klvm.mk-closure [F | Init] Return-reg C Acc)

  F Args Init [] C Acc -> (walk-tailcall F Init C Acc)
  F Args Init Return-reg C Acc -> (walk-call F Init Return-reg C Acc))

(define walk-closure
  Return-reg Args Nregs Init Body C Acc ->
  (let Ninit (length Init)
       Arity (+ Ninit (length Args))
       F (gensym klvm-lambda)
       TL (context-toplevel C)
       A (closure-args (protect A) 0 Arity [])
       Fn (context-quote C)
       . (context-toplevel-> C (walk-func regkl.closure F A Nregs Body Fn TL))
    (mk-closure [klvm.lambda F] Args Init Return-reg C Acc)))

(define walk-x3
  [type X Type] C -> (do (warn-type)
                         (walk-x3 X C))
  [regkl.reg R] C -> [klvm.reg (func-reg R C)]
  [regkl.arg R] C -> [klvm.reg (func-arg R C)]
  X C -> X where (const? X)
  X _ -> (fail))

(define qwalk-x3
  X C -> (qwalk-x3' ((context-quote C) X) X C))

(define qwalk-x3'
  skip X _ -> X
  walk X C <- (walk-x3 X C)
  _ X C -> (qsubst X C))

(define mk-args
  Args C -> (map (/. X (qwalk-x3 X C)) Args))

(define apply
  [F | Args] [] _ C Acc -> [[F | Args] | Acc]
                           where (element? F [klvm.push-error-handler
                                              klvm.pop-error-handler])
  [thaw X] Ret-reg Tail? C Acc -> (apply [X] Ret-reg Tail? C Acc)
  [F | Args] Ret-reg false C Acc -> (walk-call F Args Ret-reg C Acc)
  [F | Args] _ true C Acc -> (walk-tailcall F Args C Acc))

(define q-expr
  X [] false C Acc -> [X | Acc]
  X Dst-reg false C Acc -> [[klvm.reg-> Dst-reg X] | Acc]
  X _ true C Acc -> [[klvm.return X] | Acc])

(define qsubst
  X C <- (walk-x3 X C)
  X C -> (map (/. A (qsubst A C)) X) where (cons? X)
  X _ -> X)

(define quote'
  walk [F | Args] Dst-reg Tail? C Acc -> (let A (mk-args [F | Args] C)
                                           (apply A Dst-reg Tail? C Acc))
  skip X Dst-reg Tail? C Acc -> (q-expr X Dst-reg Tail? C Acc)
  _ X Dst-reg Tail? C Acc -> (q-expr (qsubst X C) Dst-reg Tail? C Acc))

(define quote
  X Dst-reg Tail? C Acc -> (let Q ((context-quote C) X)
                             (quote' Q X Dst-reg Tail? C Acc)))

(define walk-x2
  [type X Type] Return-reg C Acc -> (do (warn-type)
                                        (walk-x2 X Return-reg C Acc))
  
  [regkl.closure Args Nregs Init Body] Return-reg C Acc ->
  (walk-closure Return-reg Args Nregs (mk-args Init C) Body C Acc)
  
  [regkl.freeze Nregs Init Body] Return-reg C Acc ->
  (walk-closure Return-reg [] Nregs (mk-args Init C) Body C Acc)
  
  [regkl.reg R] Return-reg C Acc ->
  [[klvm.reg-> Return-reg [klvm.reg (func-reg R C)]] | Acc]
  
  [regkl.arg R] Return-reg C Acc ->
  [[klvm.reg-> Return-reg [klvm.reg (func-arg R C)]] | Acc]

  X Return-reg C Acc <- (quote X Return-reg false C Acc)
  X Return-reg C Acc -> [[klvm.reg-> Return-reg X] | Acc] where (const? X)
  X _ _ _ -> (error "Unexpected L2 REGKL expression ~S" X))

(define head'
  [] -> []
  [X | Xs] -> X)

(define walk-if-reg
  R Then Else Tail? C Acc -> (let T' (head' (walk-x1 Then false Tail? C []))
                                  E' (head' (walk-x1 Else false Tail? C []))
                                  Key (if Tail? klvm.tailif klvm.if)
                               [[Key [klvm.reg R] T' E'] | Acc]))

(define walk-if
  [regkl.reg R] Then Else Tail? C Acc ->
  (walk-if-reg (func-reg R C) Then Else Tail? C Acc)

  [regkl.arg R] Then Else Tail? C Acc ->
  (walk-if-reg (func-arg R C) Then Else Tail? C Acc)

  X _ _ _ _ _ -> (error "Broken Reg-KLambda ~S." X))

(define walk-do
  [X] Tail? C Acc -> (walk-x1 X true Tail? C Acc)
  [X | Rest] Tail? C Acc -> (let Acc' (walk-x1 X true false C Acc)
                              (walk-do Rest Tail? C Acc')))

(define in-do'
  [X] -> X
  [X | Y] -> [do | (reverse [X | Y])]
  X -> X where (= X (fail)))

(define in-do
  Fn true Acc -> (Fn Acc)
  Fn false Acc -> (let X (in-do' (Fn []))
                    (if (= X (fail))
                        X
                        [X | Acc])))

(define walk-x1
  \\X _ Tail? _ _ <- (do (output "(klvm.s1.walk-x1 ~S ~S)~%" X Tail?) (fail))
  [do | X] Do? Tail? C Acc -> (in-do (walk-do X Tail? C) Do? Acc)
  [if If Then Else] _ Tail? C Acc -> (walk-if If Then Else Tail? C Acc)
  [regkl.reg R] _ true C Acc -> [[klvm.return [klvm.reg (func-reg R C)]]
                                 | Acc]
  [regkl.reg _] _ false _ Acc -> Acc
  [regkl.arg R] _ true C Acc -> [[klvm.return [klvm.reg (func-arg R C)]]
                                 | Acc]
  [regkl.arg _] _ false _ Acc -> Acc
  [regkl.closure | _] _ false _ Acc ->  Acc
  [regkl.freeze | _ ] _ false _ Acc -> Acc

  [regkl.reg-> R X2] Do? false C Acc ->
  (in-do (walk-x2 X2 (func-reg R C) C) Do? Acc)

  [regkl.closure Args Nregs Init Body] Do? true C Acc ->
  (in-do (walk-closure [] Args Nregs (mk-args Init C) Body C) Do? Acc)

  [regkl.freeze Nregs Init Body] Do? true C Acc ->
  (in-do (walk-closure [] [] Nregs (mk-args Init C) Body C) Do? Acc)

  X Do? Tail? C Acc <- (in-do (quote X [] Tail? C) Do? Acc)

  _ _ false _ Acc -> Acc
  X _ true C Acc -> [[klvm.return X] | Acc]
  X _ _ _ _ -> (error "klvm.s1.walk-x1: Unexpected L1 REGKL expression ~S" X))

(define func-code
  regkl.func -> func
  regkl.toplevel -> toplevel
  regkl.closure -> closure)

(define walk-func
  regkl.toplevel _ _ _ [] _ Toplevel -> Toplevel
  Type Name Args Nregs Body Fn Toplevel -> 
  (let Arity (length Args)
       S (+ Nregs Arity)
       C (mk-context Name Nregs Arity S 0 Toplevel Fn)
       Body' (walk-x1 Body false true C [])
       Extra (context-frame-size-extra C)
       X [(func-code Type) Name Args S Extra | (reverse Body')]
    [X | (context-toplevel C)]))

(define walk-1
  [Type Name Args Nregs Body] Fn Toplevel -> 
  (walk-func Type Name Args Nregs Body Fn Toplevel)
  where (element? Type [regkl.func regkl.toplevel])
  [X | Y] _ _ -> (error "Unexpected toplevel expression ~S~%" [X | Y])
  X Fn Toplevel -> (walk-func
                    regkl.toplevel (gensym toplevel) [] 0 X Fn Toplevel))

(define walk-toplevel
  [] _ Acc -> (reverse Acc)
  [X | Y] Fn Acc -> (walk-toplevel Y Fn (walk-1 X Fn Acc)))

(define ensure-function
  F -> (/. _ walk) where (element? F [_ [] false])
  F -> F)

(define translate
  Fn X Elim-toplevel-atoms? ->
  (let X' (regkl.translate (map (denest.translate Fn) X)
                           Elim-toplevel-atoms?)
    (walk-toplevel X' (ensure-function Fn) []))))
