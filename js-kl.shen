(defstruct context
  (nregs number)
  (toplevel string)
  (argname symbol)
  (varname symbol))

(define max
  X Y -> X where (> X Y)
  _ Y -> Y)

(define str-js-from-shen*
  "" Acc -> Acc
  (@s "-" S) Acc -> (str-js-from-shen* S (cn Acc "_"))
  (@s "_" S) Acc -> (str-js-from-shen* S (cn Acc "$_"))
  (@s "$" S) Acc -> (str-js-from-shen* S (cn Acc "$$"))
  (@s "'" S) Acc -> (str-js-from-shen* S (cn Acc "$quote$"))
  (@s "`" S) Acc -> (str-js-from-shen* S (cn Acc "$bquote$"))
  (@s "/" S) Acc -> (str-js-from-shen* S (cn Acc "$slash$"))
  (@s "*" S) Acc -> (str-js-from-shen* S (cn Acc "$asterisk$"))
  (@s "+" S) Acc -> (str-js-from-shen* S (cn Acc "$plus$"))
  (@s "%" S) Acc -> (str-js-from-shen* S (cn Acc "$percent$"))
  (@s "=" S) Acc -> (str-js-from-shen* S (cn Acc "$eq$"))
  (@s "?" S) Acc -> (str-js-from-shen* S (cn Acc "$question$"))
  (@s "!" S) Acc -> (str-js-from-shen* S (cn Acc "$excl$"))
  (@s ">" S) Acc -> (str-js-from-shen* S (cn Acc "$gt$"))
  (@s "<" S) Acc -> (str-js-from-shen* S (cn Acc "$lt$"))
  (@s "." S) Acc -> (str-js-from-shen* S (cn Acc "$dot$"))
  (@s "|" S) Acc -> (str-js-from-shen* S (cn Acc "$bar$"))
  (@s "#" S) Acc -> (str-js-from-shen* S (cn Acc "$sharp$"))
  (@s "~" S) Acc -> (str-js-from-shen* S (cn Acc "$tilde$"))
  (@s ":" S) Acc -> (str-js-from-shen* S (cn Acc "$colon$"))
  (@s ";" S) Acc -> (str-js-from-shen* S (cn Acc "$sc$"))
  (@s "@" S) Acc -> (str-js-from-shen* S (cn Acc "$at$"))
  (@s "&" S) Acc -> (str-js-from-shen* S (cn Acc "$amp$"))
  (@s "{" S) Acc -> (str-js-from-shen* S (cn Acc "$cbraceopen$"))
  (@s "}" S) Acc -> (str-js-from-shen* S (cn Acc "$cbraceclose$"))
  (@s C S) Acc -> (str-js-from-shen* S (cn Acc C)))

(define str-js-from-shen
  X -> (cn "$shen_" (cn X "$"))
       where (element? X ["return" "new" "delete" "function" "while" "for" 
                          "var" "if" "do" "in" "super"])
  X -> (str-js-from-shen* X ""))

(define sym-js-from-shen
  X -> (intern (str-js-from-shen (str X))))

(set js-backslash (n->string 92))
(set js-dquote (n->string 34))

(define esc-string
  "" Acc -> Acc
  (@s C S) Acc -> (let P (value js-backslash)
                    (esc-string S (make-string "~A~A~A" Acc P C)))
                  where (or (= C (value js-backslash))
                            (= C (value js-dquote)))
  (@s C S) Acc -> (esc-string S (cn Acc "\\x0a"))
                  where (= (string->n C) 10)
  (@s C S) Acc -> (esc-string S (cn Acc "\\x0d"))
                  where (= (string->n C) 13)
  (@s C S) Acc -> (esc-string S (cn Acc C)))

(define cut-shen-prefix
  (@s "shen-" S) -> S
  (@s "shen_" S) -> S
  S -> S)

(set shen-js-rename-sym [load print eval read readline @p write putstr])

(set shen-nonfunc-symbol [if cond defun type lambda let freeze and or in out
                          true false where define declare])

(define func-name
  X -> (intern (cn "shen_" (cut-shen-prefix (str-js-from-shen (str X)))))
       where (and (not (element? X (value shen-nonfunc-symbol)))
                  (or (element? X (value shen-js-rename-sym))
                      (shen-sysfunc? X)
                      (value shen-*installing-kl*)))
  X -> (sym-js-from-shen X) where (symbol? X)
  X -> X)

(define esc-obj
  X -> (make-string "\"~A\"" (esc-string X "")) where (string? X)
  X -> (func-name X) where (shen-sysfunc? X)
  X -> X)

(define str-join*
  [] S R -> R
  [X | L] S "" -> (str-join* L S X)
  [X | L] S R -> (str-join* L S (make-string "~A~A~A" R S X)))

(define str-join
  X S -> (str-join* X S ""))

(define arg-list
  X -> (str-join X ", "))

(define tail-call-ret
  X -> (make-string "new Shen_tco_obj(function() {return ~A;})" X))

(define tail-call
  X -> (make-string "shen_tail_call(function() {return ~A;})" X))

(define emit-funcall
  F Args false -> (make-string "~A([~A])" F (arg-list Args))
  F Args true -> (tail-call-ret (emit-funcall F Args false)))

(define get-func-obj
  X true C -> (make-string "shen_get_fn_js(~A)" (get-func-obj X false C))
  X false C -> (func-name X) where (symbol? X)
  X false C -> X)

(define tail-call-expr
  Code C -> (js-from-kl-expr Code false C))

(define cond-case
  X C -> (tail-call-expr X C))

(define emit-cond*
  [] Tail? C -> (error "cond failure: no default branch")
  [[true E] | Cases] Tail? C -> (js-from-kl-expr E Tail? C)
  [[X E] | Cases] Tail? C -> (make-string "((~A) ? ~A : ~A)"
                                          (cond-case X C)
                                          (js-from-kl-expr E Tail? C)
                                          (emit-cond* Cases Tail? C)))

(define emit-cond
  Cases Tail? C -> (emit-cond* Cases Tail? C))

(define emit-trap-error
  X EF false C -> (let S (make-string "function() {return ~A;}"
                                      (js-from-kl-expr X false C))
                       EX (js-from-kl-expr EF false C)
                    (make-string "trap_error_js(~A, ~A)" S EX))
  X EF true C -> (tail-call-ret (emit-trap-error X EF false C)))

(define emit-func-ret
  F X Tail? C -> (let Args (map (/. X (js-from-kl-expr X false C)) X)
                   (unwind-funcall F false Args C Tail?)))

(define emit-empty
  X C -> (emit-func-ret shen-empty?-js [X] true C))

(define predicate-op
  number? X _ _ -> true where (number? X)
  string? X _ _ -> true where (string? X)
  boolean? true _ _ -> true
  boolean? false _ _ -> false
  boolean? X _ C -> (make-string "~A(~A)"
                                 (sym-js-from-shen shen-boolean?-js)
                                 (js-from-kl-expr X false C))
  string? X _ C -> (make-string "(typeof(~A) == 'string')"
                                (js-from-kl-expr X false C))
  number? X _ C -> (make-string "(typeof(~A) == 'number')"
                                (js-from-kl-expr X false C))
  symbol? X _ C -> (make-string "shen_is_type_js(~A, ~A)"
                                (js-from-kl-expr X false C)
                                "shen_type_symbol")
  cons? X _ C -> (make-string "shen_is_type_js(~A, ~A)"
                              (js-from-kl-expr X false C)
                              "shen_type_cons")
  tuple? X _ C -> (make-string "shen_is_type_js(~A, ~A)"
                               (js-from-kl-expr X false C)
                               "shen_tuple")
  vector? X _ C -> (make-string "~A(~A)"
                                (sym-js-from-shen shen-vector?-js)
                                (js-from-kl-expr X false C))
  empty? X _ C -> (emit-empty X C)
  absvector? X _ C -> (make-string "~A(~A)"
                                   (sym-js-from-shen shen-absvector?-js)
                                   (js-from-kl-expr X false C))
  _ _ _ _ -> (fail))

(define math-op
  + [X Y] _ _ -> (+ X Y) where (and (number? X) (number? Y))
  - [X Y] _ _ -> (- X Y) where (and (number? X) (number? Y))
  * [X Y] _ _ -> (* X Y) where (and (number? X) (number? Y))
  / [X Y] _ _ -> (/ X Y) where (and (number? X) (number? Y))
  Op [X Y] Tail? C -> (make-string "(~A ~A ~A)"
                                   (js-from-kl-expr X false C)
                                   Op
                                   (js-from-kl-expr Y false C))
                      where (element? Op [+ - * /])
  _ _ _ _ -> (fail))

(define equality-op
  [X Y] _ _ -> (= X Y) where (and (number? X) (number? Y))
  [X Y] _ _ -> (= X Y) where (and (string? X) (string? Y))
  [X Y] _ _ -> (= X Y) where (and (boolean? X) (boolean? Y))
  [X []] _ C -> (emit-empty X C)
  [[] Y] _ C -> (emit-empty Y C)
  [X Y] true C -> (make-string "~A(~A, ~A)"
                               (sym-js-from-shen shen-equal?-js)
                               (js-from-kl-expr X false C)
                               (js-from-kl-expr Y false C))
  [X Y] false C -> (tail-call (equality-op [X Y] true C))
  _ _ _ -> (fail))

(define order-op
  Op [X Y] _ C -> (let X (js-from-kl-expr X false C)
                       Y (js-from-kl-expr Y false C)
                    (make-string "(~A ~A ~A)" X Op Y))
                  where (element? Op [> < >= <=])
  _ _ _ _ -> (fail))

(define logic-op
  not [false] _ _ -> true
  not [true] _ _ -> false
  not [X] _ C -> (make-string "(!~A)" (js-from-kl-expr X false C))
  and [false X] _ _ -> false
  or [true X] _ _ -> true
  and [X Y] _ C ->  (make-string "(~A && ~A)"
                                 (js-from-kl-expr X false C)
                                 (js-from-kl-expr Y false C))
  or [X Y] _ C -> (make-string "(~A || ~A)"
                               (js-from-kl-expr X false C)
                               (js-from-kl-expr Y false C))
  _ _ _ _ -> (fail))

(define emit-set
  X V C -> (make-string "(shen_globals[~A[1]] = ~A)"
                        (js-from-kl-expr X false C)
                        (js-from-kl-expr V false C)))

(define emit-freeze
  Body C -> (make-string "(function () {return ~A;})"
                         (tail-call-ret (js-from-kl-expr Body true C))))

(define emit-thaw
  X false C -> (make-string "shen_tail_call(~A)"
                            (js-from-kl-expr X false C))
  X true C -> (make-string "(~A())" (js-from-kl-expr X false C)))

(define basic-op
  intern ["true"] _ _ -> "true"
  intern ["false"] _ _ -> "false"
  intern [X] _ _ -> (make-string "[shen_type_symbol, ~A]"
                                 (esc-obj (str-js-from-shen X)))
                    where (string? X)
  intern [X] _ C -> (make-string "shen_intern_js(~A)"
                                 (js-from-kl-expr X false C))
  cons [X Y] _ C -> (let X (js-from-kl-expr X false C)
                         Y (js-from-kl-expr Y false C)
                      (make-string "[shen_type_cons, ~A, ~A]" X Y))
  @p [X Y] _ C -> (let X (js-from-kl-expr X false C)
                       Y (js-from-kl-expr Y false C)
                    (make-string "[shen_tuple, ~A, ~A]" X Y))
  set [X Y] _ C -> (emit-set X Y C)
  thaw [X] Tail? C -> (emit-thaw X Tail? C)
  function [X] _ C -> (js-from-kl-expr X true C)
  hd [X] _ C -> (make-string "~A[1]" (js-from-kl-expr X false C))
  tl [X] _ C -> (make-string "~A[2]" (js-from-kl-expr X false C))
  cn [X Y] _ C -> (make-string "(~A + ~A)"
                               (js-from-kl-expr X false C)
                               (js-from-kl-expr Y false C))
  pos [X Y] _ C -> (make-string "~A[~A]"
                                (js-from-kl-expr X false C)
                                (js-from-kl-expr Y false C))
  address-> [V I X] _ C -> (make-string "shen_absvector_set_js(~A, ~A, ~A)"
                                        (js-from-kl-expr V false C)
                                        (js-from-kl-expr I false C)
                                        (js-from-kl-expr X false C))
  <-address [V I] _ C -> (make-string "shen_absvector_ref_js(~A, ~A)"
                                      (js-from-kl-expr V false C)
                                      (js-from-kl-expr I false C))
  _ _ _ _ -> (fail))

(define emit-do
  [X] Tail? C Acc -> (let Do (map (/. Y (js-from-kl-expr Y false C))
                                  (reverse Acc))
                          Sep (make-string ",~%  ")
                       (make-string "(~A,~%  ~A)"
                                    (str-join Do Sep)
                                    (js-from-kl-expr X Tail? C)))
  [X | Body] Tail? C Acc -> (emit-do Body Tail? C [X | Acc]))

(define std-op
  Pred [X] Tail? C <- (predicate-op Pred X Tail? C)
  Op X Tail? C <- (math-op Op X Tail? C)
  = X Tail? C <- (equality-op X Tail? C)
  Op X Tail? C <- (logic-op Op X Tail? C)
  Op X Tail? C <- (order-op Op X Tail? C)
  Op X Tail? C <- (basic-op Op X Tail? C)
  trap-error [X Y] Tail? C -> (emit-trap-error X Y Tail?)
  do Body Tail? C -> (emit-do Body Tail? C [])
  fail [] _ _ -> "shen_fail_obj"
  _ _ _ _ -> (fail))

(define emit-mk-func
  Name Args Code C -> (let N (func-name Name)
                           G (make-string
                               "if (~A.length < ~A) return [~A, ~A]"
                               (context-argname C)
                               (length Args)
                               N
                               (context-argname C))
                        (make-string "function ~A (~A) {~A; return ~A}"
                                     N
                                     (context-argname C)
                                     G
                                     Code)))

(define emit-mk-func-obj
  Name Args Code C -> (let N (gensym shen-user-lambda)
                           X (emit-mk-func N Args Code C)
                        (make-string "~A = [~A, []];~%" Name X)))

(define emit-mk-closure
  Args Init Code C -> (let TL (context-toplevel C)
                           C1 (mk-context 0 TL (gensym Arg) R)
                           N (gensym shen-user-lambda)
                           X (js-from-kl-expr Code true C1)
                           T1 (context-toplevel-> C (context-toplevel C1))
                           A (map (/. X (js-from-kl-expr X false C)) Init)
                        (make-string "[~A, [~A]]"
                                     (emit-mk-func N Args X C1)
                                     (str-join A ", "))))

(define emit-get-arg
  N C -> (make-string "~A[~A]" (context-argname C) N))

(define emit-set-reg
  N X C -> (let Y (js-from-kl-expr X false C)
                T (context-nregs-> C (max (+ N 1) (context-nregs C)))
             (make-string "(~A~A = ~A)" (context-varname C) N Y)))

(define emit-get-reg
  N C -> (make-string "~A~A" (context-varname C) N))

(define func-arg
  C X -> (js-from-kl-expr X false C))

(define js-from-kl-expr
  X _ _ -> "shen_fail_obj" where (= X (fail))
  true _ _ -> "true"
  false _ _ -> "false"
  [cons X Y] _ C -> (make-string "[shen_type_cons, ~A, ~A]"
                                 (js-from-kl-expr X false C)
                                 (js-from-kl-expr Y false C))
  [type Value _] Tail? C -> (js-from-kl-expr Value Tail? C)
  [cond | Cases] Tail? C -> (emit-cond Cases Tail? C)
  [if Expr Then Else] Tail? C -> (emit-cond [[Expr Then] [true Else]] Tail? C)
  [freeze X] _ C -> (emit-freeze X C)

  [shen-get-arg N] _ C -> (emit-get-arg N C)
  [shen-get-reg N] _ C -> (emit-get-reg N C)
  [shen-set-reg! N X] _ C ->  (emit-set-reg N X C)
  [shen-mk-func Name Args Code] _ C -> (emit-mk-func-obj Name Args Code C)
  [shen-mk-closure Args Env Map Code] _ C -> (emit-mk-closure
                                               Args Env Map Code C)

  [F | A] Tail? C <- (std-op F A Tail? C)
  [[S N] | A] Tail? C -> (let Args (map (func-arg C) A)
                           (emit-funcall [S N] true Args Tail?))
                         where (element? S [shen-get-arg shen-get-reg])
  [[X | Y] | A] Tail? C -> (let F (js-from-kl-expr [X | Y] false C)
                                Args (map (func-arg C) A)
                             (emit-funcall F false Args Tail? C))
  [F | Rest] Tail? C -> (let Sys? (not (shen-sysfunc? F))
                             Args (map (func-arg C) Rest)
                          (emit-funcall F Sys? Args  Tail? C))
                        where (symbol? F)
  X _ _ -> (esc-obj X))
