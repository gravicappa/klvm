\* Copyright 2010-2011 Ramil Farkhshatov

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>. *\

(package reg-kl- [shen-get-arg shen-get-reg shen-set-reg! shen-mk-closure
                  shen-mk-func shen-mk-freeze]

(defstruct context
  (toplevel s-expr)
  (nregs number))

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

(define remove-duplicates-aux
  [] Acc -> (reverse Acc)
  [X | R] Acc -> (remove-duplicates-aux R (adjoin X Acc)))

(define remove-duplicates
  X -> (remove-duplicates-aux X []))

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
  X [[X | J] | R] -> J where (>= J 0)
  X [_ | R] -> (reuse-idx X R))

(define new-var-idx-or-reuse
  X Env [] -> (new-var-idx X Env)
  X Env [U | Unused] <- (reuse-idx U Env)
  X Env [U | Unused] -> (new-var-idx-or-reuse X Env Unused))

(define add-var-aux
  X I [] Acc -> [[X | I] | (reverse Acc)]
  X I [[Y | I] | Env] Acc -> (append (reverse [[X | I] | Acc]) Env)
  X I [Y | Env] Acc -> (add-var-aux X I Env [Y | Acc]))

(define add-var
  X I Env -> (add-var-aux X I Env []))

(define max
  X Y -> X where (> X Y)
  _ Y -> Y)

(define walk-let-expr
  X V Env Used-in-body Used Unext C true
  -> (let Used' (remove X Used-in-body)
          Unext' (append Used' Unext)
          Unused (difference (map head Env) Unext')
          I (new-var-idx-or-reuse X Env Unused)
          _ (context-nregs-> C (max (+ I 1) (context-nregs C)))
          Env' (add-var X I Env)
          R (walk-expr V Env Used Unext' C)
       (@p (mk-shen-set-reg I R) Env'))
  _ V Env _ Used Unext C false -> (@p (walk-expr V Env Used Unext C) Env))

(define walk-let
  X V Body Env Used Unext C
  -> (let U (used-vars Body [X | Env])
          E? (element? X U)
          U' (if E?
                 [X | Used]
                 Used)
          R (walk-let-expr X V Env U Used Unext C E?)
          I (fst R)
          Env' (snd R)
          B (remove-do (walk-expr Body Env' (append U Unext) Unext C))
          Expr (if (cons? I)
                   [I | B]
                   B)
       [do | Expr]))

(define walk-do-aux
  [] _ [] _ _ Acc -> Acc
  [X] Env [U] Unext C Acc -> (let E (walk-expr X Env U Unext C)
                                  Acc (append Acc (remove-do E))
                               (walk-do-aux [] Env [] Unext C Acc))
  [X | Y] Env [U V | W] Unext C Acc -> (let E (walk-expr X Env U V C)
                                            Acc (append Acc (remove-do E))
                                            W [V | W]
                                         (walk-do-aux Y Env W Unext C Acc)))

(define walk-do
  X Env Used Unext C -> (let U (used-vars-cascade X Env Used)
                             E (walk-do-aux X Env U Unext C [])
                          [do | E]))

(define walk-apply-aux
  [] _ [] _ _ Acc -> (reverse Acc)
  [X] Env [U] Unext C Acc -> (let E (walk-expr X Env U Unext C)
                               (walk-apply-aux [] Env [] Unext C [E | Acc]))
  [X | Y] Env [U V | W] Unext C Acc -> (let E (walk-expr X Env U V C)
                                         (walk-apply-aux
                                           Y Env [V | W] Unext C [E | Acc])))

(define walk-apply
  X Env Used Unext C -> (let U (used-vars-cascade X Env Used)
                          (walk-apply-aux X Env U Unext C [])))

(define walk-if
  If Then Else Env Used Unext C -> (let UT (used-vars-aux Then Env [] Unext)
                                        UE (used-vars-aux Else Env [] Unext)
                                        U (append UT UE)
                                        If' (walk-expr If Env Used U C)
                                        Then' (walk-expr Then Env UT Unext C)
                                        Else' (walk-expr Else Env UE Unext C)
                                     [if If' Then' Else']))

(define walk-cond
  [] _ _ _ _ -> [error "error: cond failure"]
  [[If Then] | Else] Env Used Unext C
  -> (let UT (used-vars-aux Then Env [] Unext)
          UE (used-vars-aux Else Env [] Unext)
          UI (used-vars-aux If Env [] (append UT UE))
          Else (walk-cond Else Env Used Unext C)
       (walk-if If Then Else Env UI UE C)))

(define mk-closure-kl
  Args Init Body -> [shen-mk-closure Args Init Body])

(define mk-closure-args-init
  [] _ M -> (reverse M)
  [U | Used] Env M -> (let Y (mk-shen-get-reg (var-idx U Env))
                        (mk-closure-args-init Used Env [Y | M])))

(define mk-closure-env
  [] Acc -> Acc
  [X | U] Acc -> (mk-closure-env U [[X | (new-var-idx X Acc)] | Acc]))

(define mk-closure-list
  Args Body Env Used C -> (let Env' (mk-closure-env Used [])
                               Init (mk-closure-args-init Used Env [])
                               Code (mk-function-kl Args Body Env' C)
                            [Init Code]))

(define walk-lambda-aux
  X [lambda Y Body] Args Env Used C -> (walk-lambda-aux
                                         Y Body [X | Args] Env Used C)
  X Body Args Env Used C -> (let Args (reverse [X | Args])
                                 A (append Used (reverse [X | Args]))
                                 X (mk-closure-list A Body Env Used C)
                              [shen-mk-closure Args | X]))

(define walk-lambda
  X Code Args Env C -> (let U (used-vars [lambda X Code] Env)
                         (walk-lambda-aux X Code Args Env U C)))

(define walk-freeze
  Code Env Used C -> (let X (mk-closure-list Used Code Env Used C)
                       [shen-mk-freeze | X]))

(define lift-defun
  F Args Body C -> (let C' (mk-context (context-toplevel C) 0)
                        X (mk-defun-kl F Args Body [] C')
                        TL (context-toplevel-> C [X | (context-toplevel C')])
                     [function F]))

(define walk-expr
  [let X V Body] Env Used Unext C -> (walk-let X V Body Env Used Unext C)
  [if X Then Else] Env Used Unext C -> (walk-if X Then Else Env Used Unext C)
  [cond | B] Env Used Unext C -> (walk-cond B Env Used Unext C)
  [do | Code] Env Used Unext C -> (walk-do Code Env Used Unext C)
  [lambda X B] Env _ _ C -> (walk-lambda X B [] Env C)
  [freeze X] Env Used _ C -> (walk-freeze X Env Used C)
  [defun F Args Body] _ _ _ C -> (lift-defun F Args Body C)
  [X | A] Env Used Unext C -> (walk-apply [X | A] Env Used Unext C)
  X Env _ _ _ -> (mk-shen-get-reg (var-idx X Env))
                 where (and (var-defined? X Env) (symbol? X))
  X _ _ _ _ -> X)

(define mk-defun-env
  [] I Acc -> Acc
  [X | A] I Acc -> (mk-defun-env A (- I 1) [[X | I] | Acc]))

(define mk-function-kl
  Args Body Env C -> (let Args (remove-duplicates Args)
                          Env (mk-defun-env Args -1 Env)
                          U (used-vars Body Args)
                       (walk-expr Body Env U [] C)))

(define mk-defun-kl
  F Args Body Env C -> (let X (mk-function-kl Args Body Env C)
                         [shen-mk-func F Args X]))

(define walk-toplevel
  [defun F Args Body] Acc -> (let C (mk-context Acc 0)
                                  X (mk-defun-kl F Args Body [] C)
                               [X | (context-toplevel C)])
  X Acc -> (let C (mk-context Acc 0)
                X (walk-expr X [] [] [] C)
             [X | (context-toplevel C)]))

(define walk-aux
  [] Acc -> (reverse Acc)
  [X | R] Acc -> (walk-aux R (walk-toplevel X Acc)))

(define walk
  Exprs -> (walk-aux Exprs [])))
