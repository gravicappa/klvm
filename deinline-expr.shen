(package deinline-expr []

(define varname
  X [] -> X
  X [[X | Y] | Env] -> Y
  X [_ | Env] -> (varname X Env))

(define deinline-let
  X V Body Let Env -> (let Var (gensym (protect Shenkl))
                           A (deinline-expr V Let Env)
                           Env' [[X | Var] | Env]
                           Let' [[Var | (fst A)] | (snd A)]
                        (deinline-expr Body Let' Env')))

(define deinline-if
  If Then Else Let Env -> (let Var (gensym (protect Shenkl))
                               A (deinline-expr If Let Env)
                               Let' [[Var | (fst A)] | (snd A)]
                               Then' (deinline-aux Then Env)
                               Else' (deinline-aux Else Env)
                            (@p [if Var Then' Else'] Let')))

(define deinline-cond-aux
  [] -> [error "error: cond failure"]
  [[If Then] | Cond] -> [if If Then (deinline-cond-aux Cond)])

(define deinline-cond
  Cond Let Env -> (deinline-expr (deinline-cond-aux Cond) Let Env))

(define deinline-lambda
  X Body Let Env -> (@p [lambda X (deinline-aux Body Env)] Let))

(define deinline-freeze
  X Let Env -> (@p [freeze (deinline-aux X Env)] Let))

(define deinline-shortcut
  Op X Y Let Env F -> (deinline-expr (F X Y) Let Env))

(define and-fn
  X Y -> [if X Y false])

(define or-fn
  X Y -> [if X true Y])

(define deinline-do-aux
  [] Env Acc -> (reverse Acc)
  [X | Body] Env Acc -> (let Y (deinline-aux X Env)
                          (deinline-do-aux Body Env [Y | Acc])))

(define deinline-do
  X Let Env -> (@p [do | (deinline-do-aux X Env [])] Let))

(define deinline-expr
  [and X Y] Let Env -> (deinline-expr (and-fn X Y) Let Env)
  [or X Y] Let Env -> (deinline-expr (or-fn X Y) Let Env)
  [if If Then Else] Let Env -> (deinline-if If Then Else Let Env)
  [let X V Body] Let Env -> (deinline-let X V Body Let Env)
  [freeze X] Let Env -> (deinline-freeze X Let Env)
  [lambda X Body] Let Env -> (deinline-lambda X Body Let Env)
  [cond | Cond] Let Env -> (deinline-cond Cond Let Env)
  [defun F Args Body] Let _ -> (@p [defun F Args (deinline Body)] Let)
  [trap-error X E] Let Env -> (let Y [klvm-trap-error [freeze X] E]
                                (deinline-expr Y Let Env))
  [do | Body] Let Env -> (deinline-do Body Let Env)
  [X | Y] Let Env -> (deinline-app [X | Y] Let Env [])
  X Let Env -> (@p (varname X Env) Let))

(define deinline-app
  [] Let _ Acc -> (@p (reverse Acc) Let)
  [[X | Y] | R] Let Env Acc -> (let Var (gensym (protect Shenkl))
                                    A (deinline-expr [X | Y] Let Env)
                                    Let' [[Var | (fst A)] | (snd A)]
                                 (deinline-app R Let' Env [Var | Acc]))
                               where (not (element? X [type]))
  [F | R] Let Env Acc -> (deinline-app R Let Env [(varname F Env) | Acc]))

(define mk-let-cascade
  [] Code -> Code
  [[Var | Expr] | Vars] Code -> [let Var Expr (mk-let-cascade Vars Code)])

(define deinline-aux
  Code Env -> (let X (deinline-expr Code [] Env)
                (mk-let-cascade (reverse (snd X)) (fst X))))

(define deinline
  Code -> (deinline-aux Code []))
)
