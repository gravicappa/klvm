(package denest []

(define varname
  X [] -> X
  X [[X | Y] | Env] -> Y
  X [_ | Env] -> (varname X Env))

(define walk-let
  X V Body Let Env Fn -> (let Var (gensym (protect Shenkl))
                              A (walk-expr V Let Env Fn)
                              Env' [[X | Var] | Env]
                              Let' [[Var | (fst A)] | (snd A)]
                           (walk-expr Body Let' Env' Fn)))

(define walk-if
  If Then Else Let Env Fn -> (let Var (gensym (protect Shenkl))
                                  A (walk-expr If Let Env Fn)
                                  Let' [[Var | (fst A)] | (snd A)]
                                  Then' (walk-aux Then Env Fn)
                                  Else' (walk-aux Else Env Fn)
                               (@p [if Var Then' Else'] Let')))

(define walk-cond-aux
  [] -> [error "error: cond failure"]
  [[If Then] | Cond] -> [if If Then (walk-cond-aux Cond)])

(define walk-cond
  Cond Let Env Fn -> (walk-expr (walk-cond-aux Cond) Let Env Fn))

(define walk-lambda
  X Body Let Env Fn -> (let Env' [[X | X] | Env]
                         (@p [lambda X (walk-aux Body Env' Fn)] Let)))

(define walk-freeze
  X Let Env Fn -> (@p [freeze (walk-aux X Env Fn)] Let))

(define and-fn
  X Y -> [if X Y false])

(define or-fn
  X Y -> [if X true Y])

(define walk-do-aux
  [] Env _ Acc -> (reverse Acc)
  [X | Body] Env Fn Acc -> (let Y (walk-aux X Env Fn)
                             (walk-do-aux Body Env Fn [Y | Acc])))

(define walk-do
  X Let Env Fn -> (@p [do | (walk-do-aux X Env Fn [])] Let))

(define walk-trap
  X E Let Env Fn -> (@p [trap-error (walk-aux X Env Fn) (walk-aux E Env Fn)]
                        Let))

(define walk-expr
  [and X Y] Let Env Fn -> (walk-expr (and-fn X Y) Let Env Fn)
  [or X Y] Let Env Fn -> (walk-expr (or-fn X Y) Let Env Fn)
  [if If Then Else] Let Env Fn -> (walk-if If Then Else Let Env Fn)
  [let X V Body] Let Env Fn -> (walk-let X V Body Let Env Fn)
  [freeze X] Let Env Fn -> (walk-freeze X Let Env Fn)
  [lambda X Body] Let Env Fn -> (walk-lambda X Body Let Env Fn)
  [cond | Cond] Let Env Fn -> (walk-cond Cond Let Env Fn)
  [defun F Args Body] Let _ Fn -> (@p [defun F Args (translate Fn Body)] Let)
  [trap-error X E] Let Env Fn -> (walk-trap X E Let Env Fn)
  [do | Body] Let Env Fn -> (walk-do Body Let Env Fn)
  [X | Y] Let Env Fn -> (walk-app [X | Y] Let Env Fn [])
  X Let Env _ -> (@p (varname X Env) Let))

(define walk-app
  [] Let _ Fn Acc -> (@p (reverse Acc) Let)
  [[X | Y] | R] Let Env Fn Acc -> (let A (walk-expr [X | Y] Let Env Fn)
                                   (walk-app R (snd A) Env Fn [(fst A) | Acc]))
                                   where (Fn [X | Y])
  [[X | Y] | R] Let Env Fn Acc -> (let Var (gensym (protect Shenkl))
                                       A (walk-expr [X | Y] Let Env Fn)
                                       Let' [[Var | (fst A)] | (snd A)]
                                    (walk-app R Let' Env Fn [Var | Acc]))
                                  where (not (element? X [type]))
  [F | R] Let Env Fn Acc -> (walk-app R Let Env Fn [(varname F Env) | Acc]))

(define mk-let-cascade
  [] Code -> Code
  [[Var | Expr] | Vars] Code -> [let Var Expr (mk-let-cascade Vars Code)])

(define walk-aux
  Code Env Fn -> (let X (walk-expr Code [] Env Fn)
                   (mk-let-cascade (reverse (snd X)) (fst X))))

(define ensure-function
  F -> (/. _ false) where (= F _)
  F -> F)

(define translate
  Fn Code -> (walk-aux Code [] (ensure-function Fn))))
