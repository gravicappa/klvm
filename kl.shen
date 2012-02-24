\*

## Vector-based closures

<type fn env nargs args>

fn: (closure --> args-vector) --> A
env: <v1 v2 ... vn>
args: <a1 a2 ... an>

*\

(define mk-context
  Toplevel N -> (@v Toplevel N <>))

(define context-toplevel->
  C X -> (vector-> C 1 X))

(define context-toplevel
  C -> (<-vector C 1))

(define context-nvars->
  C X -> (vector-> C 2 X))

(define context-nvars
  C -> (<-vector C 2))

(define var-idx-aux
  X I [] -> (error "Unknown var: ~A~%" X)
  X I [[X | J] | R] -> J
  X I [_ | R] -> (var-idx-aux X (+ I 1) R))

(define var-idx
  X Env -> (var-idx-aux X 0 Env))

(define new-var-idx-aux
  _ I [] -> I
  X I [[_ | J] | R] -> (new-var-idx-aux X I R) where (< J 0)
  X I [[_ | J] | R] -> (new-var-idx-aux X (+ J 1) R) where (>= J I)
  X I [_ | R] -> (new-var-idx-aux X I R))

(define new-var-idx
  X Env -> (new-var-idx-aux X 0 Env))

(define var-defined?
  _ [] -> false
  X [[X | _] | _] -> true
  X [X | _] -> true
  X [_ | R] -> (var-defined? X R))

(define used-vars-aux
  [let X V B] G Env Acc -> (let U (used-vars-aux B G [X | Env] Acc)
                             (used-vars-aux V G Env U))
  [lambda X B] G Env Acc -> (used-vars-aux B G [X | Env] Acc)
  [X | Y] G Env Acc -> (used-vars-aux X G Env (used-vars-aux Y G Env Acc))
  X G Env Acc -> (adjoin X Acc) where (and (symbol? X)
                                           (not (var-defined? X Env))
                                           (var-defined? X G))
  _ _ _ Acc -> Acc)

(define used-vars
  X G -> (used-vars-aux X G [] []))

(define new-var-idx-or-reuse
  X Env [] -> (@p (new-var-idx X Env) [])
  X Env [U | Unused] -> (@p (var-idx U Env) Unused))

(define remove-do
  [do | X] -> X
  X -> [X])

(define used-vars-cascade-aux
  [] _ _ Acc -> Acc
  [X | Y] Env UAcc Acc -> (let U (used-vars-aux X Env [] UAcc)
                            (used-vars-cascade-aux Y Env U [U | Acc])))

(define used-vars-cascade
  X Env Used -> (used-vars-cascade-aux (reverse X) Env Used []))

(define mk-shen-set-reg
  X _ -> (error "Cannot set function argument~%") where (< X 0)
  X V -> [shen-set-reg! X V])

(define mk-shen-get-reg
  X -> [shen-get-arg (- (- 0 X) 1)] where (< X 0)
  X -> [shen-get-reg X])

(define kl-walk-let-expr
  X V Env Used C true -> (let Unused (difference (map head Env) Used)
                              I (new-var-idx-or-reuse X Env Unused)
                              Env' [[X | (fst I)] | Env]
                              R (kl-walk-expr V Env (snd I) C)
                              RE (mk-shen-set-reg (fst I) R)
                           (@p RE Env'))
  X V Env Used C false -> (@p (kl-walk-expr V Env Used C) Env))

(define kl-walk-let
  X V Body Env Used C -> (let U (used-vars Body [X | Env])
                              E? (element? X U)
                              U' (append U Used)
                              R1 (kl-walk-let-expr X V Env U' C E?)
                              E (fst R1)
                              S1 (snd R1)
                              UV (used-vars V Env)
                              B (remove-do (kl-walk-expr Body S1 UV C))
                              Expr (if (cons? E)
                                       [E | B]
                                       B)
                           [do | Expr]))

(define kl-walk-do-aux
  [] _ _ _ Acc -> Acc
  [X | Y] Env [U | V] C Acc -> (let E (kl-walk-expr X Env U C)
                                    Acc (append Acc (remove-do E))
                                 (kl-walk-do-aux Y Env V C Acc)))

(define kl-walk-do
  X Env Used C -> (let U (used-vars-cascade X Env Used)
                       E (kl-walk-do-aux X Env U C [])
                    [do | E]))

(define kl-walk-apply-aux
  [] _ _ _ Acc -> (reverse Acc)
  [X | Y] Env [U | V] C Acc -> (let E (kl-walk-expr X Env U C)
                                 (kl-walk-apply-aux Y Env V C [E | Acc])))

(define kl-walk-apply
  X Env Used C -> (let U (used-vars-cascade X Env Used)
                    (kl-walk-apply-aux X Env U C [])))

(define mk-closure-kl
  Args Env Mapping Body -> [shen-mk-closure Args Env Mapping Body])

(define mk-closure-env-mapping
  _ [] M -> M
  Env [[X | I] | Env'] M -> (let Y [(var-idx X Env) | I]
                              (mk-closure-env-mapping Env Env' [Y | M])))

(define mk-closure-env
  [] Acc -> Acc
  [X | U] Acc -> (mk-closure-env U [[X | (new-var-idx X Acc)] | Acc]))

(define kl-walk-lambda-aux
  X [lambda Y | Body] Args Env Used C -> (kl-walk-lambda-aux
                                           Y Body [X | Args] Env Used C)
  X Body Args Env Used C -> (let Args (reverse [X | Args])
                                 Env' (mk-closure-env Used [])
                                 Mapping (mk-closure-env-mapping Env Env' [])
                                 Code (mk-function-kl Args Body Env' C)
                              (mk-closure-kl Args Env' Mapping Code)))

(define kl-walk-lambda
  X Code Args Env Used C -> (let U (used-vars Code Env)
                              (kl-walk-lambda-aux X Code Args Env U C)))

(define kl-walk-expr
  [let X V Body] Env Used C -> (kl-walk-let X V Body Env Used C)
  [do | Code] Env Used C -> (kl-walk-do Code Env Used C)
  [lambda X B] Env Used C -> (kl-walk-lambda X B [] Env Used C)
  [X | A] Env Used C -> (kl-walk-apply [X | A] Env Used C)
  X Env _ _ -> (mk-shen-get-reg (var-idx X Env))
               where (and (var-defined? X Env) (symbol? X))
  X _ _ _ -> X)

(define mk-defun-env
  [] I Acc -> Acc
  [X | A] I Acc -> (mk-defun-env A (- I 1) [[X | I] | Acc]))

(define mk-function-kl
  Args Body Env C -> (let Env (mk-defun-env Args -1 Env)
                       (kl-walk-expr Body Env (used-vars Body Args) C)))

(define mk-defun-kl
  F Args Body Env C -> (let X (mk-function-kl Args Body Env C)
                         [shen-mk-func F Args X]))

(define kl-walk-toplevel
  [defun F Args Body] Acc -> (let C (mk-context Acc 0)
                                  X (mk-defun-kl F Args Body [] C)
                               [X | (context-toplevel C)])
  [lambda _ _] Acc -> Acc
  X Acc -> (let C (mk-context Acc 0)
             [(kl-walk-expr X [] [] C) | (context-toplevel C)]))

(define kl-walk-aux
  [] Acc -> (reverse Acc)
  [X | R] Acc -> (kl-walk-aux R (kl-walk-toplevel X Acc)))

(define kl-walk
  Exprs -> (kl-walk-aux Exprs []))
