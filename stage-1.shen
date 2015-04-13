(package klvm.s1 [denest.translate klvm-dump
                  regkl.translate regkl.arg regkl.reg regkl.reg->
                  regkl.closure regkl.func regkl.toplevel regkl.freeze
                  regkl.trap-error

                  klvm.native klvm.reg klvm.reg-> klvm.call klvm.tailcall
                  klvm.tailif klvm.if klvm.return klvm.mk-closure
                  klvm.push-error-handler klvm.pop-error-handler klvm.lambda]

(defstruct context
  (func symbol)
  (nregs number)
  (arity number)
  (frame-size number)
  (frame-size-extra number)
  (toplevel s-expr)
  (native (A --> context --> A)))

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

(define reg-from-arg
  [klvm.reg R | _] C -> R
  X _ -> [X])

(define regs-from-args
  [] C Acc -> Acc
  [X | Xs] C Acc -> (regs-from-args Xs C [(reg-from-arg X C) | Acc]))

(define set-call-args 
  [] _ Acc -> Acc
  [[A] | As] I Acc -> (set-call-args As (+ I 1) [[I | A] | Acc])
  [A | As] I Acc -> (set-call-args As (+ I 1) [[I | [klvm.reg A]] | Acc])
  [X | As] _ _ -> (error "Unexpected arg expr: ~S~%" X))

(define walk-call
  F Args Return-reg C Acc ->
  (let R (regs-from-args Args C [])
       Nargs (length R)
       . (upd-context-frame-size-extra Nargs C)
       X (set-call-args R 0 [])
    [[klvm.call F Nargs Return-reg (reverse X)] | Acc]))

(define place-free-args
  [] _ _ Args Acc -> (@p Acc (reverse Args))
  [[] | Xs] I Map Args Acc -> (place-free-args Xs (+ I 1) Map [[] | Args] Acc)
  [X | Xs] I Map Args Acc -> (place-free-args Xs (+ I 1) Map [X | Args] Acc)
                             where (element? I Map)
  [X | Xs] I Map Args Acc -> (let Acc [[I | X] | Acc]
                                  Args [[] | Args]
                               (place-free-args Xs (+ I 1) Map Args Acc)))

(define mk-place
  [] M -> (+ M 1)
  [[_ | I] | Xs] M -> (mk-place Xs M) where (> M I)
  [[_ | I] | Xs] M -> (mk-place Xs I))

(define place-args'
  [] _ Map Acc -> (@p Acc Map)
  [[] | Xs] I Map Acc -> (place-args' Xs (+ I 1) Map Acc)
  [X | Xs] I Map Acc -> (place-args' Xs (+ I 1) Map [[I | X] | Acc])
                        where (not (element? I Xs))
  [X | Xs] I Map Acc -> (let Place (mk-place Map 0)
                             Acc [[Place | X] | Acc]
                             Map [[I | Place] | Map]
                          (place-args' Xs (+ I 1) Map Acc)))

(define place-extra
  [] Acc -> (reverse Acc)
  [[[] | _]] Acc -> (reverse Acc)
  [[D | S] | Ps] Acc -> (place-extra Ps [[D | S] | Acc]))

(define nplaces
  [] N -> N
  [[I | _] | Ps] N -> (nplaces Ps N) where (> N I)
  [[I | _] | Ps] N -> (nplaces Ps I))

(define place-args
  Nregs Args -> (let X (place-free-args Args 0 Args [] [])
                     X (place-args' (snd X) 0 [[[] | Nregs]] (fst X))
                     X (place-extra (snd X) (fst X))
                  (@p X (nplaces X 0))))

(define set-tailcall-args'
  [] Acc -> Acc
  [[D | D] | Ps] Acc -> (set-tailcall-args' Ps Acc)
  [[D | [X]] | Ps] Acc -> (set-tailcall-args' Ps [[D | X] | Acc])
  [[D | S] | Ps] Acc -> (set-tailcall-args' Ps [[D | [klvm.reg S]] | Acc]))

(define set-tailcall-args
  Nregs Args Acc -> (let P (place-args Nregs Args)
                         . (upd-context-frame-size-extra (+ (snd P) 1))
                         Acc (set-tailcall-args' (fst P) Acc)
                      (@p Acc (snd P))))

(define walk-tailcall
  F Args C Acc ->
  (let R (regs-from-args Args C [])
       Nargs (length R)
       . (upd-context-frame-size-extra Nargs C)
       X (set-tailcall-args (context-frame-size C) R [])
    [[klvm.tailcall F Nargs (reverse (fst X))] | Acc]))

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
       Fn (context-native C)
       . (context-toplevel-> C (walk-func regkl.closure F A Nregs Body Fn TL))
    (mk-closure [klvm.lambda F] Args Init Return-reg C Acc)))

(define mk-args
  Args C -> (map (/. X (walk-x3 X C)) Args))

(define walk-native'
  X _ _ C _ -> X where (= X (fail))
  X [] false C Acc -> [[klvm.native X] | Acc]
  X Return-reg false C Acc -> [[klvm.reg-> Return-reg [klvm.native X]] | Acc]
  X _ true C Acc -> [[klvm.return [klvm.native X]] | Acc])

(define walk-native
  F Args [] _ C Acc -> [[F | Args] | Acc]
                       where (element? F [klvm.push-error-handler
                                          klvm.pop-error-handler])
  F Args Return-reg Tail? C Acc -> (let X ((context-native C) [F | Args])
                                     (walk-native' X Return-reg Tail? C Acc)))

(define walk-x3
  [type X Type] C -> (do (warn-type)
                         (walk-x3 X C))
  [regkl.reg R] C -> [klvm.reg (func-reg R C)]
  [regkl.arg R] C -> [klvm.reg (func-arg R C)]
  X C -> X where (const? X)
  X _ -> (error "Unexpected L3 Reg-KLambda ~S." X))

(define walk-apply
  [F | Args] Ret-reg Tail? C Acc <- (walk-native F Args Ret-reg Tail? C Acc)
  [thaw X] Ret-reg Tail? C Acc -> (walk-apply [X] Ret-reg Tail? C Acc)
  [F | Args] Ret-reg false C Acc -> (walk-call F Args Ret-reg C Acc)
  [F | Args] _ true C Acc -> (walk-tailcall F Args C Acc))

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

  [F | Args] Return-reg C Acc ->
    (walk-apply (mk-args [F | Args] C) Return-reg false C Acc)

  X Return-reg C Acc -> [[klvm.reg-> Return-reg X] | Acc] where (const? X)
  X _ _ _ -> (error "Unexpected L2 Reg-KLambda expression ~S" X))

(define head*
  [] -> []
  [X | Xs] -> X)

(define walk-if-reg
  R Then Else Tail? C Acc -> (let T' (head* (walk-x1 Then false Tail? C []))
                                  E' (head* (walk-x1 Else false Tail? C []))
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
  [X | Y] -> [do | (reverse [X | Y])])

(define in-do
  Fn true Acc -> (Fn Acc)
  Fn false Acc -> [(in-do' (Fn [])) | Acc])

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

  [F | Args] Do? Tail? C Acc ->
  (in-do (walk-apply (mk-args [F | Args] C) [] Tail? C) Do? Acc)

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

(define ensure-native
  [] -> (/. X (fail))
  X -> (/. X (fail)) where (= X _)
  Fn -> Fn)

(define translate
  Denest-fn Fn X Elim-toplevel-atoms? ->
  (let X' (regkl.translate (map (denest.translate Denest-fn) X)
                           Elim-toplevel-atoms?)
    (walk-toplevel X' (ensure-native Fn) []))))
