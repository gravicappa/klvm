(package klvm [-type walk subst]
(define mktype
  Name Code -> [define (concat Name -type) | Code])

(define mktrans
  Name Code -> [define Name | Code])

(define prim-code
  [Pat Code] Acc -> (append [Code -> | (reverse Pat)] Acc)
  [Pat Code Where] Acc -> (append [Where where Code -> | (reverse Pat)] Acc))

(define prim-type
  [Pat _ | Where] Type Acc -> (prim-code [Pat Type | Where] Acc))

(define prim'
  [] [] _ Name Types Trans -> 
  [package null []
           (mktype Name (reverse Types)) (mktrans Name (reverse Trans))]

  [] [[T | Code] | Def] _ Name Types Trans ->
  (prim' Code Def T Name Types Trans)

  [X | Xs] Def T Name Types Trans ->
  (prim' Xs Def T Name (prim-type X T Types) (prim-code X Trans)))
  
(defcc <item>
  X := X;)

(defcc <where>
  X := X;)

(defcc <args>
  <item> <args> := [<item> | <args>] where (not (= <item> ->));
  <e> := [];)

(defcc <def>
  <args> -> <item> where <where> := [<args> <item> <where>];
  <args> -> <item> := [<args> <item>];)

(defcc <defs>
  <def> <defs> := [<def> | <defs>];
  <e> := [];)

(define collect-prim
  [] Acc -> (reverse Acc)
  [[Type | Defs] | Rest] Acc -> (let X (compile (function <defs>) Defs)
                                  (collect-prim Rest [[Type | X] | Acc])))

(define defprim
  Name Code -> (prim' [] (collect-prim Code []) _ Name [] []))

(define walk-klvm
  Prim Type Level X -> (walk-klvm' (Type X) Prim Type X Level))

(define walk-klvm'
  subst P T [X | Xs] L -> (P [X | (map (walk-klvm P T (+ L 1)) Xs)] L)
  walk P T [X | Xs] L -> (P [X | (map (walk-klvm P T (+ L 1)) Xs)] L)
  _ P _ X L -> (P X L)))

(defmacro klvm.defprim-macro
  [klvm.define-primitives Name | Code] -> (klvm.defprim Name Code))
