(package klvm-test [klvm-from-kl
                    kl-from-shen
                    backend-utils.with-file-output
                    backend-utils.str-from-sexpr
                    klvm-trans.null-fn
                    klvm-trans.pretty-print-code

                    regkl.trap-error
                    klvm.call-error-handler 
                    klvm.push-error-handler
                    klvm.pop-error-handler
                    klvm.error-unwind-get-handler
                    klvm.current-error

                    klvm.s1.walk
                    ]

(set *shen-code1* "

(define list-len-aux
  [] Acc -> Acc
  [X | Xs] Acc -> (list-len-aux Xs (+ Acc 1)))

(define list-len
  List -> (list-len-aux List 0))

(define test-partial
  -> ((+ 2) 7))

(define adder
  Add -> (/. X (+ Add X)))

(define reversex-aux
  [] Acc -> Acc
  [X | Y] Acc -> (reversex-aux Y [X | Acc]))

(define reversex
  List -> (reversex-aux List []))

(define mapx-aux
  _ [] Acc -> (reversex-aux Acc [])
  Fn [X | List] Acc -> (mapx-aux Fn List [(Fn X) | Acc]))

(define mapx
  Fn List -> (mapx-aux Fn List []))

(define incr-list
  List -> (mapx (+ 1) List))

(define test-trap-error -> (trap-error (- 1 1) (/. E 10)))
(define test-trap-error-2 -> (trap-error (/ 10 0) (/. E 0)))

(define test-trap-error-3
  -> (let X (+ 17 10)
          X (* X 2)
          X (trap-error (/ X 0) (/. E (+ X 1)))
       (- X 55)))

(define test-freeze
  -> (let X 5
          F (freeze (* X 5))
          X 6
       (thaw F)))

(define test-gensym -> (gensym (protect XYZ)))

(define appendx-aux
  [] [] Acc -> (reversex Acc)
  [] [Y | Ys] Acc -> (appendx-aux [] Ys [Y | Acc])
  [X | Xs] Y Acc -> (appendx-aux Xs Y [X | Acc]))

(define appendx
  X Y -> (appendx-aux X Y []))

(define mkcurry
  F 0 -> [freeze F]
  F -1 -> F
  F N -> (mkcurry-aux F N []))
  
(define mkcurry-aux
  F 0 Vars -> [F | Vars]
  F N Vars -> (let X (gensym (protect V))
                [/. X (mkcurry-aux F (- N 1) (appendx Vars [X]))]))

")

(set *code1* [

[defun test-if [X]
  [if X
      yes
      no]]

[defun test-ret-elim [X]
  [let A [if X
             yes
             no]
    X]]

[defun list-len-aux [V10659 V10660]
  [cond [[= [] V10659] V10660]
        [[cons? V10659] [list-len-aux [tl V10659] [+ V10660 1]]]
        [true [shen.f_error list-len-aux]]]]

[defun list-len [V10661]
  [list-len-aux V10661 0]]

[defun test-partial []
  [[+ 2] 7]]

[defun adder [V10181]
  [lambda X [+ V10181 X]]]

[defun part1 [X]
  [+ X]]

[defun part2 [X]
  [let A [+ X]
    A]]

[defun test-partial-2 []
  [part2 10 20]]

[defun test-partial-3 []
  [part1 10 20]]

[defun test-closure []
  [let X [adder 6]
    [X 8]]]

[defun test-tail-call []
  [let X [adder 6]
    [let Y [X 8]
      [- Y 14]]]]

[defun reversex-aux [List Acc]
  [cond [[= [] List] Acc]
        [true [reversex-aux [tl List] [cons [hd List] Acc]]]]]

[defun reversex [List]
  [reversex-aux List []]]

[defun mapx-aux [V41907 V41908 V41909]
  [cond [[= [] V41908] [reversex-aux V41909 []]]
        [[cons? V41908]
         [mapx-aux V41907 [tl V41908] [cons [V41907 [hd V41908]] V41909]]]
        [true [shen.f_error mapx-aux]]]]

[defun mapx [V41910 V41911]
  [mapx-aux V41910 V41911 []]]

[defun incr-list [V10182]
  [mapx [+ 1] V10182]]

[defun incr-list-aux [V10182]
  [mapx [lambda X [+ 1 X]] V10182]]

[defun regkl.trap-error [X E]
  [do [klvm.push-error-handler E]
      [let R [X]
        [do [klvm.pop-error-handler]
            R]]]]

[defun test-trap-error []
  [trap-error [- 1 1] [lambda E 10]]]

[defun test-trap-error-2 []
  [trap-error [undefined-func 1 0] [lambda E 0]]]

[defun test-trap-error-3 []
  [let X [+ 17 10]
    [let X [* X 2]
      [let X [trap-error [+ X 10] [lambda E [+ X 1]]]
        [- X 64]]]]]

[defun test-trap-error-4 []
  [let X [+ 17 10]
    [let X [* X 2]
      [let X [trap-error [undefined-func  X 0] [lambda E [+ X 1]]]
        [- X 55]]]]]

[defun test-call [X]
  [+ 7 [+ 7 X]]]

[defun test-map [List]
  [mapx [lambda X [> X 0]] List]]

[defun test-map-aux [List]
  [mapx-aux [lambda X [> X 0]] List []]]

[let X 7
  [let Y 19
    [set ret [+ X Y]]]]

[+ 8 10]

"Koo!"

[defun test-toplevel []
  [value ret]]

[defun test-reg-kl []
  [let X [regkl.walk
          [cons [cons defun [cons one [cons [cons V []] [cons V []]]]] []]
          false]
    X]]

[defun test-freeze []
  [let X 5
    [let F [freeze [* X 5]]
      [let X 6 [thaw F]]]]]

[defun guard-aux [V3816 V3817]
  [cond [[= [] V3816] [reversex V3817]]
        [[and [cons? V3816] [string? [hd V3816]]]
         [guard-aux [tl V3816] [cons [cons str [cons [hd V3816] []]] V3817]]]
        [[and [cons? V3816] [number? [hd V3816]]]
         [guard-aux [tl V3816] [cons [cons num [cons [hd V3816] []]] V3817]]]
        [[and [cons? V3816] [symbol? [hd V3816]]]
         [guard-aux [tl V3816] [cons [cons sym [cons [hd V3816] []]] V3817]]]
        [true [shen.f_error guard-aux]]]]

[defun test-guard []
  [guard-aux [cons 1 [cons two [cons "three" [cons "four" [cons 5 [cons six
                                                                     []]]]]]]
         []]]

[defun test-gensym [] [gensym XYZ]]

[defun appendx-aux [V6375 V6376 V6377]
  [cond [[and [= [] V6375] [= [] V6376]] [reversex V6377]]
        [[and [= [] V6375] [cons? V6376]]
         [appendx-aux [] [tl V6376] [cons [hd V6376] V6377]]]
        [[cons? V6375] [appendx-aux [tl V6375] V6376 [cons [hd V6375] V6377]]]
        [true [shen.f_error appendx-aux]]]]

[defun appendx [V6378 V6379] [appendx-aux V6378 V6379 []]]

[defun mkcurry [V6380 V6381]
  [cond [[= 0 V6381] [cons freeze [cons V6380 []]]]
        [[= -1 V6381] V6380]
        [true [mkcurry-aux V6380 V6381 []]]]]

[defun mkcurry-aux [V6382 V6383 V6384]
  [cond [[= 0 V6383] [cons V6382 V6384]]
        [true [let X [gensym V]
                [cons /. [cons X [cons [mkcurry-aux V6382
                                                [- V6383 1]
                                                [appendx V6384 [cons X []]]]
                                       []]]]]]]]

[defun test-mkcurry []
  [let X [mkcurry func 5]
    X]]

[defun test-freeze-2-aux [V7347 V7348]
  [let X 8
    [let Y [+ V7347 V7348]
      [freeze [+ V7347 [* X Y]]]]]]

[defun test-freeze-2 []
  [let F [test-freeze-2-aux 8 1]
    [thaw F]]]

[defun test-do []
  [let V [vector 3]
    [do [vector-> V 1 3]
        [do [vector-> V 2 [+ [<-vector V 1] 2]]
            [do [vector-> V 3 [* [<-vector V 2] 3]]
                V]]]]]

[defun test-let-1 [V9851]
  [let V1 [let V [vector 4]
            [if V9851
                [do [vector-> V 1 3]
                    [do [vector-> V 2 [+ [<-vector V 1] 2]]
                        [do [vector-> V 3 [* [<-vector V 2] 3]]
                            V]]]
                [do [vector-> V 1 0]
                    [do [vector-> V 2 [+ [<-vector V 1] 1]]
                        [do [vector-> V 3 [+ [<-vector V 2] 1]]
                            V]]]]]
    [do [vector-> V1 4 one]
        V1]]]

[defun list-5 [V10723 V10724 V10725 V10726 V10727]
  [cons V10723
        [cons V10724
              [cons V10725
                    [cons V10726
                          [cons V10727 []]]]]]]

[defun test-closure-2 []
  [let X [list-5 1 2]
    [let Y [X 3 4]
      [Y 5]]]]

[defun list-5+2 [V12320 V12321 V12322 V12323 V12324 V12325 V12326]
  [cons V12320
        [cons V12321
              [cons V12322
                    [cons V12323
                          [cons V12324
                                [cons V12325
                                      [cons V12326 []]]]]]]]]

[defun call-help [V12327 V12328 V12329 V12330]
  [cond [[= [] V12328] [V12327 V12329 V12330]]
        [[cons? V12328]
         [call-help [V12327 [hd V12328]] [tl V12328] V12329 V12330]]
        [true [shen.f_error call-help]]]]

[defun test-closure-3 []
  [call-help [function list-5+2] [cons 1 [cons 2 [cons 3 [cons 4 [cons 5 []]]]]] a b]]

[defun test-closure-4 []
  [let X 5
    [let F [lambda A [+ A X]]
      [let G [lambda F [lambda X [F X]]]
        [G F 12]]]]]

[defun test-closure-5 []
  [let X 5
    [let F [lambda A [- A X]]
      [F 5]]]]

[defun test-and [V4113 V4114] [and V4113 V4114]]
[defun test-or [V4115 V4116] [or V4115 V4116]]
[defun test-and-2 [V4113 V4114] [[and V4113] V4114]]
[defun test-or-2 [V4115 V4116] [[or V4115] V4116]]
])


(define update-code
  -> (let Shen (read-from-string (value *shen-code1*))
          Kl (kl-from-shen Shen)
       (set *code1* Kl)))

(define compile-code1
  -> (let Klvm (klvm-from-kl klvm-trans.null-fn (value *code1*))
       (backend-utils.with-file-output
        "test1.klvm"
        (klvm-trans.pretty-print-code Klvm))))

(define compile-stage1
  -> (let Klvm (klvm.s1.walk klvm-trans.null-fn (value *code1*))
          . (output "~%**** gotklvm~%~%")
       (backend-utils.with-file-output
        "test1.klvm1"
        (klvm-trans.pretty-print-code Klvm)
        \\(/. F (pr (backend-utils.str-from-sexpr "~R" Klvm) F))
        )))


(define find-func'
  Fn [] -> []
  Fn [[defun Fn | Rest] | _]  -> [defun Fn | Rest]
  Fn [X | Y] -> (find-func' Fn Y))

(define find-func
  Fn -> (find-func' Fn (value *code1*)))

\\(compile-code1)
)

(define klvm-test.regkl
  X -> (regkl.walk (map (function denest.walk) X) false))

(define klvm-test.s1/s
  S -> (klvm-test.s1/s (read-from-string S)) where (string? S)
  Kl -> (let RK (regkl.walk (map (function denest.walk) Kl) false)
             . (output "REGKL:~%~S~%~%" RK)
             S1 (klvm.bytecode.asm.walk Kl 3)
             . (output "KLVM.Stage-1:~%~S~%~%" S1)
          true))

(define klvm-test.mk-const
  -> (vector-> (vector 1) 1 []))

(define klvm-test.ensure-const
  X Type C -> (let R (klvm.bytecode.ensure-const X Type (<-vector C 1))
                   . (vector-> C 1 (snd R))
                (fst R)))

(define klvm-test.consts
  -> (let C (klvm-test.mk-const)
          . (klvm-test.ensure-const one sym C)
          . (output "~S~%" C)
          . (output "one: ~S~%" (klvm-test.ensure-const one sym C))
          . (klvm-test.ensure-const 2 num C)
          . (output "~S~%" C)
          . (output "one: ~S~%" (klvm-test.ensure-const one sym C))
        true))
          
