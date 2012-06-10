\*

## Vector-based closures

<type fn env nargs args>

fn: (closure --> args-vector) --> A
env: <v1 v2 ... vn>
args: <a1 a2 ... an>

*\

(package reg-kl- [shen-mk-func shen-get-arg shen-get-reg
                  shen-set-reg! shen-mk-closure]

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

(define reuse-idx
  X [] -> (fail)
  X [[X | J] | R] -> (@p J X) where (>= J 0)
  X [_ | R] -> (reuse-idx X R))

(define new-var-idx-or-reuse
  X Env [] -> (@p (new-var-idx X Env) [])
  X Env [U | Unused] <- (reuse-idx U Env)
  X Env [U | Unused] -> (new-var-idx-or-reuse X Env Unused))

(define walk-let-expr
  X V Env Used C true -> (let Unused (difference (map head Env) Used)
                              I (new-var-idx-or-reuse X Env Unused)
                              Env' [[X | (fst I)] | Env]
                              Unused1 (remove (snd I) Unused)
                              R (walk-expr V Env Unused1 C)
                              RE (mk-shen-set-reg (fst I) R)
                           (@p RE Env'))
  X V Env Used C false -> (@p (walk-expr V Env Used C) Env))

(define walk-let
  X V Body Env Used C -> (let U (used-vars Body [X | Env])
                              E? (element? X U)
                              U' (append U Used)
                              R1 (walk-let-expr X V Env U' C E?)
                              E (fst R1)
                              S1 (snd R1)
                              UV (used-vars V Env)
                              B (remove-do (walk-expr Body S1 UV C))
                              Expr (if (cons? E)
                                       [E | B]
                                       B)
                           [do | Expr]))

(define walk-do-aux
  [] _ _ _ Acc -> Acc
  [X | Y] Env [U | V] C Acc -> (let E (walk-expr X Env U C)
                                    Acc (append Acc (remove-do E))
                                 (walk-do-aux Y Env V C Acc)))

(define walk-do
  X Env Used C -> (let U (used-vars-cascade X Env Used)
                       E (walk-do-aux X Env U C [])
                    [do | E]))

(define walk-apply-aux
  [] _ _ _ Acc -> (reverse Acc)
  [X | Y] Env [U | V] C Acc -> (let E (walk-expr X Env U C)
                                 (walk-apply-aux Y Env V C [E | Acc])))

(define walk-apply
  X Env Used C -> (let U (used-vars-cascade X Env Used)
                    (walk-apply-aux X Env U C [])))

(define mk-closure-kl
  Args Init Body -> [shen-mk-closure Args Init Body])

(define mk-closure-args-init
  [] _ M -> (reverse M)
  [U | Used] Env M -> (let Y (mk-shen-get-reg (var-idx U Env))
                        (mk-closure-args-init Used Env [Y | M])))

(define mk-closure-env
  [] Acc -> Acc
  [X | U] Acc -> (mk-closure-env U [[X | (new-var-idx X Acc)] | Acc]))

(define walk-lambda-aux
  X [lambda Y | Body] Args Env Used C -> (walk-lambda-aux
                                           Y Body [X | Args] Env Used C)
  X Body Args Env Used C -> (let Args (append Used (reverse [X | Args]))
                                 Env' (mk-closure-env Used [])
                                 Init (mk-closure-args-init Used Env [])
                                 Code (mk-function-kl Args Body Env' C)
                              (mk-closure-kl Args Init Code)))

(define walk-lambda
  X Code Args Env Used C -> (let U (used-vars Code Env)
                              (walk-lambda-aux X Code Args Env U C)))

(define walk-expr
  [let X V Body] Env Used C -> (walk-let X V Body Env Used C)
  [do | Code] Env Used C -> (walk-do Code Env Used C)
  [lambda X B] Env Used C -> (walk-lambda X B [] Env Used C)
  [X | A] Env Used C -> (walk-apply [X | A] Env Used C)
  X Env _ _ -> (mk-shen-get-reg (var-idx X Env))
               where (and (var-defined? X Env) (symbol? X))
  X _ _ _ -> X)

(define mk-defun-env
  [] I Acc -> Acc
  [X | A] I Acc -> (mk-defun-env A (- I 1) [[X | I] | Acc]))

(define mk-function-kl
  Args Body Env C -> (let Env (mk-defun-env Args -1 Env)
                       (walk-expr Body Env (used-vars Body Args) C)))

(define mk-defun-kl
  F Args Body Env C -> (let X (mk-function-kl Args Body Env C)
                         [shen-mk-func F Args X]))

(define walk-toplevel
  [defun F Args Body] Acc -> (let C (mk-context Acc 0)
                                  X (mk-defun-kl F Args Body [] C)
                               [X | (context-toplevel C)])
  [lambda _ _] Acc -> Acc
  X Acc -> (let C (mk-context Acc 0)
             [(walk-expr X [] [] C) | (context-toplevel C)]))

(define walk-aux
  [] Acc -> (reverse Acc)
  [X | R] Acc -> (walk-aux R (walk-toplevel X Acc)))

(define walk
  Exprs -> (walk-aux Exprs [])))
