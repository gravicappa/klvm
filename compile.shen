(defstruct jump-entry
  (from number)
  (off number)
  (to number))

(defstruct context
  (func symbol)
  (nregs number)
  (nargs number)
  (jumps (list jump-entry))
  (table table)
  (number-constants (list number))
  (string-constants (list string))
  (bytecode binary.buf)
  (toplevel binary.buf))

(define warning
  X -> (output "Warning: ~A" X))

(define <-label-code
  L C -> (<-vector (context-labels-code C) (+ L 1)))

(define tbl->
  Domain Key X C -> (put Domain Key X (context-table C)))

(define <-tbl
  Domain Key C -> (get Domain Key (context-table C)))

(define fill-vector
  _ [] V -> V
  I [X | List] V -> (let . (vector-> V I X)
                      (fill-vector (+ I 1) List V)))

(define vector-from-list
  List -> (fill-vector 1 List (vector (length List))))

(define number-bytes
  X -> 1 where (< X 256)
  X -> 2 where (< X 65536)
  X -> 3 where (< X 16777216)
  X -> 4 where (< X 4294967296))

(define vector-index'
  I _ X V -> I where (= (<-vector V I) X)
  N N X V -> -1
  I N X V -> (vector-index' (+ I 1) N X V))

(define vector-index
  X V -> (vector-index' 1 (limit V) X V))

(define opcode
  X -> (klvm.opcode-num X))

(define mk-const'
  X [] I -> I
  X [X | S] I -> I
  X [Y | S] I -> (mk-const' X S (+ I 1)))

(define mk-number-const
  X C -> (mk-const' X (context-number-constants C) 0))

(define string-length'
  S N -> (trap-error (do (pos S N)
                         (string-length' S (+ N 1)))
                     (/. E N)))

(define string-length
  S -> (string-length' S 0))

(define mk-string-const
  X C -> (mk-const' X (context-string-constants C) 0))

(define put-opcode
  X C -> (binary.put-u8 (opcode X) (context-bytecode C)) where (symbol? X)
  X C -> (binary.put-u8 X (context-bytecode C)) where (number? X))

(define put-int1
  X C -> (do (put-opcode klvm.op_const-i8 C)
             (binary.put-i8 X (context-bytecode C))))

(define put-int2
  X C -> (do (put-opcode klvm.op_const-i16 C)
             (binary.put-i16 X (context-bytecode C))))

(define put-int4
  X C -> (do (put-opcode klvm.op_const-i32 C)
             (binary.put-i32 X (context-bytecode C))))

(define put-int
  X C -> (binary.put-u8 X (context-bytecode C))
         where (and (>= X 0) (< X 128))
  X C -> (put-int1 X C) where (<= (binary.uint-size X) 1)
  X C -> (put-int2 X C) where (<= (binary.uint-size X) 2)
  X C -> (put-int4 X C) where (<= (binary.uint-size X) 4)
  X _ -> (error "Bignums are unsupported yet. (~A)" X))

(define choose-tbl-suffix
  X -> 8 where (<= (binary.uint-size X) 1)
  X -> 16 where (<= (binary.uint-size X) 2)
  X -> 32 where (<= (binary.uint-size X) 4)
  X -> (error "Const table is too large"))
  
(define put-tbl-const
  Idx Prefix C -> (let S (choose-tbl-suffix Idx)
                       . (put-opcode (concat Prefix S) C)
                    ((concat binary.put-u S) Idx (context-bytecode C))))

(define put-string
  X C -> (put-tbl-const (mk-string-const X C) klvm.op_const-str- C))

(define put-symbol
  X C -> (put-tbl-const (mk-string-const X C) klvm.op_const-sym- C))

(define put-float
  X C -> (error "Floats are not supported yet"))

(define put-const
  X C -> (put-int X C) where (integer? X)
  X C -> (put-float X C) where (number? X)
  X C -> (put-string X C) where (string? X)
  X C -> (put-symbol X C) where (symbol? X)
  X _ -> (error "Unknown const: ~A" X))

(define comp-1arg
  Op X C -> (do (comp-expr2 X C)
                (put-opcode Op C)))

(define comp-klvm-reg
  Reg C -> (do (put-opcode klvm.op_reg C)
               (put-int Reg C)))

(define comp-klvm-func-ptr
  Name C -> (let . (put-opcode klvm.op_func-ptr C)
                 Idx (find-closure Name C)
              (put-int Idx C)))

(define const?
  X -> true where (symbol? X)
  X -> true where (string? X)
  X -> true where (number? X)
  _ -> false)

(define comp-klvm-reg->
  Dst [klvm-reg Src] C -> (do (put-opcode klvm.op_reg->reg C)
                              (put-int Dst C)
                              (put-int Src C)) where false
  Dst X C -> (do (comp-expr2 X C)
                 (put-opcode klvm.op_reg-> C)
                 (put-int Dst C)))

(define func-reg X C -> (+ (context-nargs C) X))
(define func-arg X C -> (- (context-nargs C) (+ X 1)))

(define warn-type
  -> (warning "`type` expression is not supported yet"))

(define comp-call-args Args Nargs+ C Acc ->
  (let R (regs-from-args Args [])
       Nargs (length R)
       Acc [[klvm-next-> (+ (context-label C) 1)] | Acc]
       Acc [[klvm-nregs-> [(+ (context-stack-size C) Nargs) | Nargs+]] | Acc]
       Acc [[klvm-nargs-> Nargs] | Acc]
       Acc (set-call-args R (context-stack-size C) Acc)
    Acc))

(define comp-call F Args Return-reg C ->
  (let R (klvm-trans.regs-from-args Args [])

(define closure Return-reg Args Nregs Init Code C ->
  (let Ninit (length Init)
       Nargs (+ Ninit (length Args))
       F (gensym klvm-lambda)
       TL (context-toplevel C)
       A (klvm-trans.closure-args (protect A) 0 Nargs [])
       . (context-toplevel-> C (comp-func shen-closure F A Nregs Code TL))
    (if (= Return-reg [])
        (comp-tailcall F Init C)
        (comp-call F Init Return-reg C))))

(define comp-freeze
  Tgt-reg Nregs Init Code C -> (closure Tgt-reg [] Nregs Init Code C))

(define comp-expr3
  [type X Type] C -> (do (warn-type)
                         (comp-expr3 X C))
  [shen-get-reg R] C -> (comp-reg (func-reg R C) C)
  [shen-get-arg R] C -> (comp-arg (func-arg R C) C)
  X C -> (comp-const X C) where (const? X)
  X _ -> (fail))

(define comp-expr2
  [type X Type] Return-reg C -> (do (warn-type)
                                    (comp-expr2 X Return-reg C))
  
  [shen-closure Args Nregs Init Code] Return-reg C ->
  (closure Return-reg Args Nregs Init Code C)

  [shen-freeze Nregs Init Code] Return-reg C ->
  (comp-freeze Return-reg Nregs Init Code C)
  
  [F | Args] Return-reg C -> (comp-call F Args Return-reg C)
  X Return-reg C -> (comp-load-const X C) where (const? X)
  X _ _ -> (error "Unexpected L2 Reg-KLambda expression ~S" X))

(define comp-if
  [[shen-get-reg R] Then Else] Tail? C -> ...

  X _ _ -> (error "Broken Reg-KLambda ~S." X))

(define comp-do
  [X] Tail? C -> (comp-expr1 X Tail? C)
  [X | Rest] Tail? C -> (do (comp-expr1 X false C)
                            (comp-do Rest Tail? C)))

(define comp-expr1 
  [do | X] Tail? C -> (comp-do X Tail? C)
  [if | X] Tail? C -> (comp-if X Tail? C)
  [shen-get-reg R] true C -> (comp-return-reg (func-reg R C) C)
  [shen-get-reg R] false C -> true
  [shen-set-reg! R X2] false C -> (comp-expr2 X2 (func-reg R C) C)
  [shen-get-arg R] true C -> (comp-return-reg (func-arg R C) C)
  [shen-get-arg R] false C -> true

  [shen-closure Args Nregs Init Code] true C ->
  (closure [] Args Nregs Init Code C)

  [shen-closure Args Nregs Init Code] false _ ->  true
  [shen-freeze Nregs Init Code] true C -> (comp-freeze [] Nregs Init Code C)
  [shen-freeze Nregs Init Code] false _ -> true
  [klvm-push-error-handler E] _ C -> (comp-push-error-handler E C)
  [klvm-pop-error-handler] _ C -> (put-opcode klvm.pop-error-handler C)
  [F | Args] true C -> (comp-tailcall F Args C)
  [F | Args] false C -> (comp-call F Args [] C)
  X false _ -> true
  X true C -> (comp-return-const X C)
  X _ _ -> (error "Unexpected L1 Reg-KLambda expression ~S" X))

(define comp-exprs
  [] C -> []
  [X | Y] C -> (let . (comp-expr1 X true C)
                 (comp-exprs Y C)))

(define comp-label
  L Code C -> (let . (context-label-> C L)
                (comp-exprs Code C)))

(define comp-labels
  N N C -> []
  I N C -> (let I+1 (+ I 1)
                . (comp-label I (<-vector (context-labels-src C) I+1) C)
                . (vector-> (context-labels-code C) I+1 (context-bytecode C))
                . (context-bytecode-> C (binary.mkbuf 16))
             (comp-labels (+ I 1) N C)))

(define place-labels
  [] C -> []
  [[[klvm-label I] | Code] | Labels] C ->
  (let . (vector-> (context-labels-src C) (+ I 1) Code)
    (place-labels Labels C)))

(define calc-label-offset'
  To To Off _ -> Off
  From To Off C -> (let Code (<-vector (context-labels-code C) (+ From 1))
                        Off (+ Off (binary.buf-size Code))
                     (calc-label-offset' (+ From 1) To Off C)))

(define calc-label-offset
  To To C -> 0
  From To C -> (calc-label-offset' (+ From 1) To 0 C))

(define jump-distance
  Entry C -> (let From (jump-entry-from Entry)
                  Off (jump-entry-off Entry)
                  To (jump-entry-to Entry)
                  From-code (<-vector (context-labels-code C) From)
                  L-off (calc-label-offset From To C)
                (+ L-off (- (binary.buf-size From-code) Off))))

(define fix-jump
  Dist Off Code -> (do (binary.buf-cut Off 4 Code)
                       (binary.bytevector-u8->
                        (binary.buf-buf Code) Off (- Dist 4)))
                   where (<= (- Dist 4) 255)
  Dist Off Code -> (let D (- Dist 2)
                        . (binary.buf-cut Off 2 Code)
                        Op (opcode klvm.op_const-u16)
                        B (binary.buf-buf Code)
                        . (binary.bytevector-u8-> B Off Op)
                        . (binary.bytevector-u16le-> B (+ Off 1) D)
                     true)
                   where (<= (- Dist 2) 65536)
  Dist Off Code -> (let B (binary.buf-buf Code)
                        . (binary.bytevector-u32le-> B (+ Off 1) Dist)
                     true))

(define fix-jumps
  [] C -> []
  [E | Jumps] C -> (let From (jump-entry-from E)
                        Off (jump-entry-off E)
                        Code (<-vector (context-labels-code C) From)
                        Dist (jump-distance E C)
                        . (fix-jump (jump-distance E C) Off Code)
                     (fix-jumps Jumps C)))

(define put-constants
  C -> true)

(define put-func
  Type C -> (let Buf (context-toplevel C)
                 Name (str (context-func C))
                 Name-len (string-length Name)
                 Code-size (total-labels-size C)
                 . (binary.put-u8 (opcode Type) Buf)
                 . (binary.put-u32 Name-len Buf)
                 . (binary.put-string Name Buf)
                 . (put-constants C)
                 . (output "Code-size: ~A~%" Code-size)
                 . (binary.put-u32 Code-size Buf)
                 . (binary.maybe-realloc-buf Code-size Buf)
                 \\. (put-func-labels C)
               true))

(define func-opcode
  shen-func -> klvm.op_func
  shen-toplevel -> klvm.op_toplevel
  shen-closure -> klvm.op_closure)

(define comp-func
  Type Name Args Nregs Code Bytecode -> 
  (let Nlabels (+ (max-label-idx Code 0) 1)
       C (mk-context Name Nregs (length Args) [] (vector 256) [] []
                     (binary.mkbuf 16) Bytecode)
    (do (comp-code Code C)
        \\(fix-jumps (context-jumps C) C)
        (put-func (func-opcode Type) C)
        (context-toplevel C))))

(define compile-1
  [Type Name Args Nregs Code] Bytecode -> 
  (comp-func Type Name Args Nregs Code Bytecode)
  where (element? Type [shen-func shen-toplevel shen-closure])
  [X | Y] _ -> (error "Unexpected toplevel expression ~S~%" [X | Y])
  X _ -> _)

(define compile-code
  [] Bytecode -> (context-toplevel Bytecode)
  [X | Y] C -> (do (compile-1 X Bytecode)
                   (compile-code Y Bytecode)))

(define klvm.compile
  X -> (compile-code X (binary.mkbuf 16)))

(define write-code-to-file'
  _ N N _ -> true
  F I N B -> (let . (write-byte (binary.<-bytevector (binary.buf-buf B) I) F)
               (write-code-to-file' F (+ I 1) N B)))

(define write-code-to-file
  File B -> (let F (open File out)
                 . (write-code-to-file' F 1 (+ (binary.buf-size B) 1) B)
                 . (close F)
              true))
