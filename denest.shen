(package denest []

(define varname
  X [] -> X
  X [[X | Y] | Env] -> Y
  X [_ | Env] -> (varname X Env))

(define walk-let
  X V Body Let Env -> (let Var (gensym (protect Shenkl))
                           A (walk-expr V Let Env)
                           Env' [[X | Var] | Env]
                           Let' [[Var | (fst A)] | (snd A)]
                        (walk-expr Body Let' Env')))

(define walk-if
  If Then Else Let Env -> (let Var (gensym (protect Shenkl))
                               A (walk-expr If Let Env)
                               Let' [[Var | (fst A)] | (snd A)]
                               Then' (walk-aux Then Env)
                               Else' (walk-aux Else Env)
                            (@p [if Var Then' Else'] Let')))

(define walk-cond-aux
  [] -> [error "error: cond failure"]
  [[If Then] | Cond] -> [if If Then (walk-cond-aux Cond)])

(define walk-cond
  Cond Let Env -> (walk-expr (walk-cond-aux Cond) Let Env))

(define walk-lambda
  X Body Let Env -> (let Env' [[X | X] | Env]
                      (@p [lambda X (walk-aux Body Env')] Let)))

(define walk-freeze
  X Let Env -> (@p [freeze (walk-aux X Env)] Let))

(define walk-shortcut
  Op X Y Let Env F -> (walk-expr (F X Y) Let Env))

(define and-fn
  X Y -> [if X Y false])

(define or-fn
  X Y -> [if X true Y])

(define walk-do-aux
  [] Env Acc -> (reverse Acc)
  [X | Body] Env Acc -> (let Y (walk-aux X Env)
                          (walk-do-aux Body Env [Y | Acc])))

(define walk-do
  X Let Env -> (@p [do | (walk-do-aux X Env [])] Let))

(define walk-trap
  X E Let Env -> (@p [trap-error (walk-aux X Env) (walk-aux E Env)] Let))

(define walk-expr
  [and X Y] Let Env -> (walk-expr (and-fn X Y) Let Env)
  [or X Y] Let Env -> (walk-expr (or-fn X Y) Let Env)
  [if If Then Else] Let Env -> (walk-if If Then Else Let Env)
  [let X V Body] Let Env -> (walk-let X V Body Let Env)
  [freeze X] Let Env -> (walk-freeze X Let Env)
  [lambda X Body] Let Env -> (walk-lambda X Body Let Env)
  [cond | Cond] Let Env -> (walk-cond Cond Let Env)
  [defun F Args Body] Let _ -> (@p [defun F Args (walk Body)] Let)
  [trap-error X E] Let Env -> (walk-trap X E Let Env)
  [do | Body] Let Env -> (walk-do Body Let Env)
  [X | Y] Let Env -> (walk-app [X | Y] Let Env [])
  X Let Env -> (@p (varname X Env) Let))

(define walk-app
  [] Let _ Acc -> (@p (reverse Acc) Let)
  [[X | Y] | R] Let Env Acc -> (let Var (gensym (protect Shenkl))
                                    A (walk-expr [X | Y] Let Env)
                                    Let' [[Var | (fst A)] | (snd A)]
                                 (walk-app R Let' Env [Var | Acc]))
                               where (not (element? X [type]))
  [F | R] Let Env Acc -> (walk-app R Let Env [(varname F Env) | Acc]))

(define mk-let-cascade
  [] Code -> Code
  [[Var | Expr] | Vars] Code -> [let Var Expr (mk-let-cascade Vars Code)])

(define walk-aux
  Code Env -> (let X (walk-expr Code [] Env)
                (mk-let-cascade (reverse (snd X)) (fst X))))

(define walk
  Code -> (walk-aux Code []))
)
