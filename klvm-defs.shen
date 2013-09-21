(package klvm [opcodes
               
               klvm-goto
               klvm-reg
               klvm-a-set
               klvm-reg2
               klvm-nregs->
               klvm-inc-nargs
               klvm-label
               klvm-dec-nargs
               klvm-stack->
               klvm-toplevel
               klvm-b-set
               klvm-if
               klvm-const-sym
               klvm-call
               klvm-closure
               klvm-const-u16
               klvm-nargs>0
               klvm-stack
               klvm-inc-stack-ptr
               klvm-pop-error-handler
               klvm-func
               klvm-fail
               klvm-push-error-handler
               klvm-nargs->
               klvm-inc-nargs-by-closure-nargs
               klvm-a-add
               klvm-closure-func
               klvm-put-closure-args
               klvm-mk-closure
               klvm-reg->
               klvm-closure->
               klvm-dec-stack-ptr
               klvm-stack-size
               klvm-b-add
               klvm-const-i16
               klvm-func-exit
               klvm-closure-nargs
               klvm-const-i32
               klvm-const-str
               klvm-error-unwind-get-handler
               klvm-push-extra-args
               klvm-nargs-cond
               klvm-pop-extra-args
               klvm-stack2
               klvm-return
               klvm-func-entry
               klvm-const-i8
               klvm-nargs
               klvm-goto-if
               klvm-current-error
               klvm-func-obj

               ]

(define opcode-num
  klvm-toplevel -> 128
  klvm-func -> 129
  klvm-closure -> 130

  klvm-goto -> 131
  klvm-goto-if -> 132
  klvm-call -> 133
  klvm-nargs>0 -> 134
  klvm-nargs-cond -> 135
  klvm-return -> 136
  klvm-func-exit -> 137
  klvm-func-entry -> 138
  klvm-func-obj -> 139

  klvm-a-set -> 140
  klvm-a-add -> 141
  klvm-b-set -> 142
  klvm-b-add -> 143

  klvm-nregs-> -> 144
  klvm-reg-> -> 145
  klvm-reg -> 146
  klvm-reg2 -> 147

  klvm-const-sym -> 148
  klvm-const-str -> 149
  klvm-const-i8 -> 150
  klvm-const-u16 -> 151
  klvm-const-i16 -> 152
  klvm-const-i32 -> 153

  klvm-stack-size -> 154
  klvm-stack-> -> 155
  klvm-stack -> 156
  klvm-stack2 -> 157
  klvm-inc-stack-ptr -> 158
  klvm-dec-stack-ptr -> 159

  klvm-nargs-> -> 160
  klvm-nargs -> 161
  klvm-inc-nargs -> 162
  klvm-dec-nargs -> 163
  klvm-inc-nargs-by-closure-nargs -> 164
  klvm-push-extra-args -> 165
  klvm-pop-extra-args -> 166

  klvm-closure-> -> 167
  klvm-closure-nargs -> 168
  klvm-closure-func -> 169
  klvm-put-closure-args -> 170
  klvm-mk-closure -> 171

  klvm-pop-error-handler -> 172
  klvm-push-error-handler -> 173
  klvm-error-unwind-get-handler -> 174
  klvm-current-error -> 175

  klvm-fail -> 176
  X -> (error "Unknown operation ~A" X))
)
