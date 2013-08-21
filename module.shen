(register-module [[name: klvm]
                  [author: "Ramil Farkshatov"]
                  [license: "GPLv3+"]
                  [desc: "KLambda to KLVM code translator"]
                  [depends: defstruct]
                  [load: "reg-kl.shen" "deinline-expr.shen"
                         "klvm-trans.shen" "klvm-trans-call.shen"]])
