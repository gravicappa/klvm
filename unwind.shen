(define kl-varname
  X [] -> X
  X [[X | Y] | Env] -> Y
  X [_ | Env] -> (kl-varname X Env))

(define kl-unwind-let
  X V Body Let Env -> (let Var (gensym (protect Shenkl))
                           A (kl-unwind-expr V Let Env)
                           Env' [[X | Var] | Env]
                           Let' [[Var | (fst A)] | (snd A)]
                        (kl-unwind-expr Body Let' Env')))

(define kl-unwind-if
  If Then Else Let Env -> (let Var (gensym (protect Shenkl))
                               A (kl-unwind-expr If Let Env)
                               Let' [[Var | (fst A)] | (snd A)]
                               Then' (kl-unwind-aux Then Env)
                               Else' (kl-unwind-aux Else Env)
                            (@p [if Var Then' Else'] Let')))

(define kl-unwind-cond-aux
  [] -> [error "error: cond failure"]
  [[If Then] | Cond] -> [if If Then (kl-unwind-cond-aux Cond)])

(define kl-unwind-cond
  Cond Let Env -> (kl-unwind-expr (kl-unwind-cond-aux Cond) Let Env))

(define kl-unwind-lambda
  X Body Let Env -> (@p [lambda X (kl-unwind-aux Body Env)] Let))

(define kl-unwind-freeze
  X Let Env -> (@p [freeze (kl-unwind-aux X Env)] Let))

(define kl-unwind-shortcut
  Op X Y Let Env F -> (kl-unwind-expr (F X Y) Let Env))

(define kl-and-fn
  X Y -> [if X Y false])

(define kl-or-fn
  X Y -> [if X true Y])

(define kl-unwind-do-aux
  [] Env Acc -> (reverse Acc)
  [X | Body] Env Acc -> (let Y (kl-unwind-aux X Env)
                          (kl-unwind-do-aux Body Env [Y | Acc])))

(define kl-unwind-do
  X Let Env -> (@p [do | (kl-unwind-do-aux X Env [])] Let))

(define kl-unwind-expr
  [and X Y] Let Env -> (kl-unwind-expr (kl-and-fn X Y) Let Env)
  [or X Y] Let Env -> (kl-unwind-expr (kl-or-fn X Y) Let Env)
  [if If Then Else] Let Env -> (kl-unwind-if If Then Else Let Env)
  [let X V Body] Let Env -> (kl-unwind-let X V Body Let Env)
  [freeze X] Let Env -> (kl-unwind-freeze X Let Env)
  [lambda X Body] Let Env -> (kl-unwind-lambda X Body Let Env)
  [cond | Cond] Let Env -> (kl-unwind-cond Cond Let Env)
  [defun F Args Body] Let _ -> (@p [defun F Args (kl-unwind Body)] Let)
  [trap-error X E] Let Env -> (let Y [klvm-trap-error [freeze X] E]
                                (kl-unwind-expr Y Let Env))
  [do | Body] Let Env -> (kl-unwind-do Body Let Env)
  [X | Y] Let Env -> (kl-unwind-app [X | Y] Let Env [])
  X Let Env -> (@p (kl-varname X Env) Let))

(define kl-unwind-app
  [] Let _ Acc -> (@p (reverse Acc) Let)
  [[X | Y] | R] Let Env Acc -> (let Var (gensym (protect Shenkl))
                                    A (kl-unwind-expr [X | Y] Let Env)
                                    Let' [[Var | (fst A)] | (snd A)]
                                 (kl-unwind-app R Let' Env [Var | Acc]))
                               where (not (element? X [type]))
  [F | R] Let Env Acc -> (kl-unwind-app R Let Env [(kl-varname F Env) | Acc]))

(define kl-mk-let-cascade
  [] Code -> Code
  [[Var | Expr] | Vars] Code -> [let Var Expr (kl-mk-let-cascade Vars Code)])

(define kl-unwind-aux
  Code Env -> (let X (kl-unwind-expr Code [] Env)
                (kl-mk-let-cascade (reverse (snd X)) (fst X))))

(define kl-unwind
  Code -> (kl-unwind-aux Code []))
