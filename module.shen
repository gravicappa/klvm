(register-module [[author: "Ramil Farkshatov"]
                  [license: "GPLv3+"]
                  [desc: "KLambda to KLVM code translator"]
                  [depends: "defstruct" "binary"]
                  [load: "regkl.shen" "denest.shen"
                         "stage-1.shen" "stage-2.shen"
                         "util.shen"
                         "bytecode-macro.shen"
                         "bytecode.shen"
                         "bytecode-asm.shen"
                         "bytecode-bin.shen"
                         ]])
