(package klvm.s1 [denest.walk klvm-dump
                  regkl.walk regkl.get-arg regkl.get-reg regkl.set-reg!
                  regkl.closure regkl.func regkl.toplevel regkl.freeze

                  klvm.native klvm.reg klvm.reg-> klvm.call klvm.tailcall
                  klvm.tailif klvm.if klvm.return klvm.mk-closure]

(defstruct context
  (func symbol)
  (nregs number)
  (nargs number)
  (stack-size number)
  (stack-size-extra number)
  (toplevel s-expr)
  (native (A --> context --> A)))

(define warning X -> (output "Warning: ~A" X))
(define warn-type -> (warning "`type` expression is not supported yet"))

(define const?
  X -> true where (symbol? X)
  X -> true where (string? X)
  X -> true where (number? X)
  X -> true where (boolean? X)
  _ -> false)

(define func-reg X C -> (+ (context-nargs C) X))
(define func-arg X C -> (- (context-nargs C) (+ X 1)))
(define func-next-reg C -> (- (context-stack-size C) 1))

(define upd-context-stack-size-extra
  N C -> (context-stack-size-extra-> C N)
         where (> N (context-stack-size-extra C))
  _ C -> C)

(define reg-from-arg
  [regkl.get-reg R | _] C -> (func-reg R C)
  [regkl.get-arg R | _] C -> (func-arg R C)
  X _ -> [X])

(define regs-from-args
  [] C Acc -> Acc
  [X | Xs] C Acc -> (regs-from-args Xs C [(reg-from-arg X C) | Acc]))

(define set-call-args 
  [] _ Acc -> Acc
  [A | As] I Acc -> (let X [klvm.reg A]
                      (set-call-args As (+ I 1) [[klvm.reg-> I X] | Acc]))
                    where (number? A)
  [[A] | As] I Acc -> (set-call-args As (+ I 1) [[klvm.reg-> I A] | Acc])
  [X | As] _ _ -> (error "Unexpected arg expr: ~S~%" X))

(define call-args
  [] -> []
  X -> [do | (reverse X)])

(define walk-call
  F Args Return-reg C Acc ->
  (let R (regs-from-args Args C [])
       Nargs (length R)
       . (upd-context-stack-size-extra Nargs C)
       X (set-call-args R (context-stack-size C) [])
    [[klvm.call (walk-x3 F C) Nargs Return-reg (call-args X)] | Acc]))

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
  [[D | [X]] | Ps] Acc -> (let Acc [[klvm.reg-> D X] | Acc]
                            (set-tailcall-args' Ps Acc))
  [[D | S] | Ps] Acc -> (let Acc [[klvm.reg-> D [klvm.reg S]] | Acc]
                          (set-tailcall-args' Ps Acc)))

(define set-tailcall-args
  Nregs Args Acc -> (let P (place-args Nregs Args)
                         . (upd-context-stack-size-extra (+ (snd P) 1))
                         Acc (set-tailcall-args' (fst P) Acc)
                      (@p Acc (snd P))))

(define walk-tailcall
  F Args C Acc ->
  (let R (regs-from-args Args C [])
       Nargs (length R)
       . (upd-context-stack-size-extra Nargs C)
       X (set-tailcall-args (context-stack-size C) R [])
    [[klvm.tailcall (walk-x3 F C) Nargs (call-args (fst X))] | Acc]))

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
       Nargs (+ Ninit (length Args))
       F (gensym klvm-lambda)
       TL (context-toplevel C)
       A (closure-args (protect A) 0 Nargs [])
       Fn (context-native C)
       . (context-toplevel-> C (walk-func regkl.closure F A Nregs Body Fn TL))
    (mk-closure F Args Init Return-reg C Acc)))

(define prep-native-args
  Args C -> (freeze (map (/. X (walk-x3 X C)) Args)))

(define walk-native'
  X _ _ -> X where (= X (fail))
  X [] Acc -> [[klvm.native X] | Acc]
  X Return-reg Acc -> [[reg-> Return-reg [klvm.native X]] | Acc])

(define walk-native
  F Args Return-reg C Acc -> (let Args' (prep-native-args Args)
                                  X ((context-native C) F Args')
                               (walk-native' X Return-reg Acc)))

(define walk-freeze
  Tgt-reg Nregs Init Body C Acc ->
  (walk-closure Tgt-reg [] Nregs Init Body C Acc))

(define walk-x3
  [type X Type] C -> (do (warn-type)
                         (walk-x3 X C))
  [regkl.get-reg R] C -> [klvm.reg (func-reg R C)]
  [regkl.get-arg R] C -> [klvm.reg (func-arg R C)]
  X C -> X where (const? X)
  X _ -> (error "Unexpected L3 Reg-KLambda ~S." X))

(define walk-x2
  [type X Type] Return-reg C Acc -> (do (warn-type)
                                        (walk-x2 X Return-reg C Acc))
  
  [regkl.closure Args Nregs Init Body] Return-reg C Acc ->
    (walk-closure Return-reg Args Nregs Init Body C Acc)
  
  [regkl.freeze Nregs Init Body] Return-reg C Acc ->
    (walk-freeze Return-reg Nregs Init Body C Acc)
  
  [regkl.get-reg R] Return-reg C Acc ->
    [[klvm.reg-> Return-reg [klvm.reg (func-reg R C)]] | Acc]
  
  [regkl.get-arg R] Return-reg C Acc ->
    [[klvm.reg-> Return-reg [klvm.reg (func-arg R C)]] | Acc]

  [F | Args] Return-reg C Acc <- (walk-native F Args Return-reg C Acc)
  [F | Args] Return-reg C Acc -> (walk-call F Args Return-reg C Acc)

  X Return-reg C Acc -> [[klvm.reg-> Return-reg X] | Acc] where (const? X)
  X _ _ _ -> (error "Unexpected L2 Reg-KLambda expression ~S" X))

(define walk-if-reg
  R Then Else Tail? C Acc -> (let T' (head (walk-x1 Then false Tail? C []))
                                  E' (head (walk-x1 Else false Tail? C []))
                                  Key (if Tail? klvm.tailif klvm.if)
                               [[Key [klvm.reg R] T' E'] | Acc]))

(define walk-if
  [regkl.get-reg R] Then Else Tail? C Acc ->
  (walk-if-reg (func-reg R C) Then Else Tail? C Acc)

  [regkl.get-arg R] Then Else Tail? C Acc ->
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
  [do | X] Do? Tail? C Acc -> (in-do (walk-do X Tail? C) Do? Acc)
  [if If Then Else] _ Tail? C Acc -> (walk-if If Then Else Tail? C Acc)
  [regkl.get-reg R] _ true C Acc -> [[klvm.return [klvm.reg (func-reg R C)]
                                                  (func-next-reg C)]
                                     | Acc]
  [regkl.get-reg _] _ false _ Acc -> Acc
  [regkl.get-arg R] _ true C Acc -> [[klvm.return [klvm.reg (func-arg R C)]
                                                  (func-next-reg C)]
                                     | Acc]
  [regkl.get-arg _] _ false _ Acc -> Acc
  [regkl.closure | _] _ false _ Acc ->  Acc
  [regkl.freeze | _ ] _ false _ Acc -> Acc

  [regkl.set-reg! R X2] Do? false C Acc ->
  (in-do (walk-x2 X2 (func-reg R C) C) Do? Acc)

  [regkl.closure Args Nregs Init Body] Do? true C Acc ->
  (in-do (walk-closure [] Args Nregs Init Body C) Do? Acc)

  [regkl.freeze Nregs Init Body] Do? true C Acc ->
  (in-do (walk-freeze [] Nregs Init Body C) Do? Acc)

  [F | Args] Do? true C Acc -> (in-do (walk-tailcall F Args C) Do? Acc)
  [F | Args] Do? false C Acc -> (in-do (walk-call F Args [] C) Do? Acc)
  _ _ false _ Acc -> Acc
  X _ true C Acc -> [[klvm.return X (func-next-reg C)] | Acc]
  X _ _ _ _ -> (error "Unexpected L1 Reg-KLambda expression ~S" X))

(define func-code
  regkl.func -> func
  regkl.toplevel -> toplevel
  regkl.closure -> closure)

(define stack-size
  Nregs Nargs -> (+ Nregs Nargs 2))

(define walk-func
  Type Name Args Nregs Body Fn Toplevel -> 
  (let Nargs (length Args)
       S (stack-size Nregs Nargs)
       C (mk-context Name Nregs Nargs S 0 Toplevel Fn)
       Body' (walk-x1 Body false true C [])
       Extra (context-stack-size-extra C)
       X [(func-code Type) Name Args S Extra | (reverse Body')]
    [X | (context-toplevel C)]))

(define walk-1
  [Type Name Args Nregs Body] Fn Toplevel -> 
  (walk-func Type Name Args Nregs Body Fn Toplevel)
  where (element? Type [regkl.func regkl.toplevel regkl.closure])
  [X | Y] _ _ -> (error "Unexpected toplevel expression ~S~%" [X | Y])
  X _ Toplevel -> Toplevel)

(define walk-toplevel'
  [] _ Acc -> (reverse Acc)
  [X | Y] Fn Acc -> (walk-toplevel' Y Fn (walk-1 X Fn Acc)))

(define ensure-native
  [] -> (/. X Y (fail))
  X -> (/. X Y (fail)) where (= X _)
  Fn -> Fn)

(define walk
  Fn X -> (let X' (regkl.walk (map (function denest.walk) X) false)
            (walk-toplevel' X' (ensure-native Fn) []))))
