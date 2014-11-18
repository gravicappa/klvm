(define *asm* (mk-vm))

(define-record-type asm-code
  (mk-asm-code code frame-size nregs)
  asm-code?
  (code asm-code-code)
  (frame-size asm-code-frame-size)
  (nregs asm-code-nregs))

(define (read-asm code vm)
  (define (fn nregs nregs+ body)
    (mk-asm-code (list->vector body) nregs (+ nregs nregs+)))
  (define (mk-func expr)
    (apply (lambda (name args nregs nregs+ body)
             (mk-vm-closure name (length args) (fn nregs nregs+ body) '()))
           (cdr expr)))
  (let loop ((code code)
             (toplevel '()))
    (if (pair? code)
        (let ((expr (car code)))
          (case (car expr)
            ((klvm.s1.func)
             (vm-add-func! (mk-func expr) vm)
             (loop (cdr code) toplevel))
            ((klvm.s1.closure)
             (let ((f (mk-func expr)))
               (table-set! (vm-closures vm) (vm-closure-name f) f)
               (loop (cdr code) toplevel)))
            ((klvm.s1.toplevel)
             (loop (cdr code) (cons (mk-func expr) toplevel))))))))

(define (read-asm-from-file file vm)
  (let ((data (with-input-from-file file read)))
    (read-asm data vm)))

(define (asm-closure-code fn)
  (asm-code-code (vm-closure-code fn)))

(define (asm-closure-frame-size fn)
  (asm-code-frame-size (vm-closure-code fn)))

(define (asm-closure-nregs fn)
  (asm-code-nregs (vm-closure-code fn)))

(define (asm-next-reg fn vm)
  (- (asm-closure-frame-size fn) 1))

(define (asm-nargs-reg fn vm)
  (- (asm-closure-frame-size fn) 2))

(define (asm-prev-sp-reg fn vm)
  (- (asm-closure-frame-size fn) 3))

(define (asm-func-entry fn vm)
  (log/pp `(asm-func-entry: ,fn asm: ,(asm-code? (vm-closure-code fn))))
  (cond ((asm-code? (vm-closure-code fn))
         (let ((nargs (vm-nargs vm))
               (arity (vm-closure-arity fn)))
           (log/pp `(arity: ,arity nargs: ,nargs))
           (cond ((< nargs arity)
                  (let ((func (vm-func-obj (vm-closure-name fn)
                                           arity
                                           (vm-closure-code fn)
                                           vm)))
                    (vm-wipe vm 0)
                    (set-vm-ret! vm func)
                    #t))
                 ((> nargs arity)
                  (set-vm-sp! vm (+ (vm-sp vm) (- nargs arity)))
                  (set-vm-nargs! vm (- nargs arity))
                  (vm-nregs-> vm (asm-closure-nregs fn))
                  (vm-regs-set! vm (asm-next-reg fn vm) (vm-next vm))
                  (vm-regs-set! vm (asm-nargs-reg fn vm) (vm-nargs vm))
                  ;(vm-regs-set! vm (asm-prev-sp-reg fn vm) (vm-prev-sp vm))
                  #f)
                 (#t
                  (set-vm-nargs! vm (- nargs arity))
                  (vm-nregs-> vm (asm-closure-nregs fn))
                  (log/pp `(entry sp: ,(vm-sp vm)
                            nargs-reg: ,(asm-nargs-reg fn vm)
                            next-reg: ,(asm-next-reg fn vm)
                            prev-sp-reg: ,(asm-prev-sp-reg fn vm)))
                  (vm-regs-set! vm (asm-next-reg fn vm) (vm-next vm))
                  (vm-regs-set! vm (asm-nargs-reg fn vm) (vm-nargs vm))
                  ;(vm-regs-set! vm (asm-prev-sp-reg fn vm) (vm-prev-sp vm))
                  #f))))
        (#t (error `(Unknown function object ,fn)))))

(define (asm-ensure-pc x)
  (if (pair? x)
      x
      (cons x 0)))

(define (asm-vm-next vm)
  (asm-ensure-pc (vm-next vm)))

(define (asm-run pp vm)
  (define (goto-next)
    (let ((x (asm-vm-next vm)))
      (if (vm-end-marker? (car x))
          #f
          (asm-run-code (car x) (cdr x) vm))))
  (log/pp `(asm-run ,pp))
  (cond ((null? pp) #f)
        ((vm-end-marker? pp) #f)
        ((vm-closure? pp)
         (cond ((procedure? (vm-closure-code pp))
                ((vm-closure-code pp) #f #f)
                (goto-next))
               (#t
                (cond ((asm-func-entry pp vm)
                       (vm-wipe vm 0)
                       (goto-next))
                      (#t (asm-run-code pp 0 vm))))))
        (#t #f)))

(define (asm-run-code pp pi vm)
  (define (closure-code pp pi)
    (if (and (vm-closure? pp) (asm-code? (vm-closure-code pp)))
        (vector-ref (asm-closure-code pp) pi)
        #f))
  
  (define op #f)
  (define closure #f)
  (define tmp-reg-nargs #f)
  
  (define (dec-sp-maybe pp)
    (log/pp `(dec-sp-maybe ,pp))
    (cond ((and (vm-closure? pp) (asm-code? (vm-closure-code pp)))
           (log/pp `(dec-sp))
           (set-vm-sp! vm (- (vm-sp vm) (asm-closure-frame-size pp))))))

  (define (put-closure-args closure)
    (let ((n (+ (vm-nargs vm) (length (vm-closure-vars closure)))))
      (vm-nregs-> vm n)
      (vm-put-args (vm-closure-vars closure) (+ (vm-sp vm) (vm-nargs vm)) vm)
      (set-vm-nargs! vm n)))
  
  (define (call)
    (set-vm-next! vm (cons pp (+ pi 1)))
    (set-vm-prev-sp! vm (vm-sp vm))
    (set-vm-sp! vm (+ (vm-sp vm) (asm-closure-frame-size pp)))
    (put-closure-args closure)
    (asm-run closure vm))
  
  (define (tail-call)
    (log/pp `(tmpnargs: ,tmp-reg-nargs sp: ,(vm-sp vm)))
    (set-vm-sp! vm (- (vm-sp vm) tmp-reg-nargs))
    (vm-wipe vm (vm-nargs vm))
    (put-closure-args closure)
    (asm-run closure vm))
  
  (define (ret x)
    (log/pp `(ret pp: ,pp
              sp: ,(vm-sp vm)
              nargs-reg: ,(asm-nargs-reg pp vm)
              prev-sp-reg: ,(asm-prev-sp-reg pp vm)
              regs: ,(vm-regs vm)))
    (set-vm-nargs! vm (vm-regs-ref vm (asm-nargs-reg pp vm)))
    (set-vm-prev-sp! vm (vm-regs-ref vm (asm-prev-sp-reg pp vm)))
    (cond ((positive? (vm-nargs vm))
           (set-vm-next! vm (vm-regs-ref vm (asm-next-reg pp vm)))
           (set-vm-prev-sp! vm (vm-regs-ref vm (asm-prev-sp-reg pp vm)))
           (vm-put-args (vm-closure-vars x) (vm-sp vm) vm)
           (set-vm-sp! vm (- (vm-sp vm) (vm-nargs vm)))
           (set-vm-nargs! vm (+ (vm-nargs vm) (length (vm-closure-vars x))))
           (vm-wipe vm (vm-nargs vm))
           (log/pp `(ret prev-sp: ,(vm-prev-sp vm)))
           (log/pp `(ret x: ,x))
           (asm-run x vm))
          (#t
           (let ((pc (asm-ensure-pc (vm-regs-ref vm (asm-next-reg pp vm)))))
             (set-vm-ret! vm x)
             ;(set-vm-sp! vm (vm-prev-sp vm))
             (vm-wipe vm 0)
             (cond ((vm-end-marker? (car pc)) #f)
                   (#t
                    (dec-sp-maybe (car pc))
                    (jmp* (car pc) (cdr pc))))))))
  
  (define (ret-reg reg)
    (ret (vm-regs-ref vm reg)))
  
  (define (ret-fn x)
    (ret (table-ref (vm-closures vm) x)))
  
  (define (ret-const x)
    (ret (case x
           ((true) #t)
           ((false) #f)
           (else x))))
  
  (define (tail-fn-> fn nargs)
    (set! closure fn)
    (set! tmp-reg-nargs (vm-regs-ref vm (asm-nargs-reg pp vm)))
    (set-vm-nargs! vm (+ nargs tmp-reg-nargs))
    (set-vm-next! vm (vm-regs-ref vm (asm-next-reg pp vm))))
  
  (define (load-const reg x)
    (let ((x (cond ((eq? x 'true) #t)
                   ((eq? x 'false) #f)
                   ((symbol? x) (table-ref (vm-fns vm) x x))
                   (#t x))))
      (vm-regs-set! vm reg x)))
  
  (define (jmp to-pi)
    (set! pi to-pi)
    (set! op (vector-ref (asm-closure-code pp) pi))
    (loop))
  
  (define (jmp* to-pp to-pi)
    (set! pp to-pp)
    (set! pi to-pi)
    (set! op (vector-ref (asm-closure-code pp) pi))
    (loop))
  
  (define (loop)
    (vm-show-step vm (cons pp pi) "STEP")
    (log/pp `(pp ,pp type: ,(cond ((symbol? pp) 'symbol)
                                  ((procedure? pp) 'proc)
                                  ((vm-closure? pp) 'closure)
                                  (#t 'unknown))))
    (log/pp `(op: ,op))
    (case (car op)
      ((klvm.load-reg->)
       (vm-regs-set! vm (cadr op) (vm-regs-ref vm (caddr op)))
       (jmp (+ pi 1)))
      ((klvm.load-ret->)
       (dec-sp-maybe pp)
       (vm-regs-set! vm (cadr op) (vm-ret vm))
       (jmp (+ pi 1)))
      ((klvm.drop-ret)
       (dec-sp-maybe pp)
       (jmp (+ pi 1)))
      ((klvm.load-fn->)
       (vm-regs-set! vm
                     (cadr op)
                     (table-ref (vm-closures vm) (caddr op)))
       (jmp (+ pi 1)))
      ((klvm.load-const->)
       (load-const (cadr op) (caddr op))
       (jmp (+ pi 1)))
      ((klvm.closure-reg->)
       (set! closure (vm-regs-ref vm (cadr op)))
       (set-vm-nargs! vm (caddr op))
       (jmp (+ pi 1)))
      ((klvm.closure-fn->)
       (set! closure (vm-fn-ref vm (cadr op)))
       (set-vm-nargs! vm (caddr op))
       (jmp (+ pi 1)))
      ((klvm.closure-lambda->)
       (set-vm-nargs! vm (caddr op))
       (set! closure (table-ref (vm-closures vm) (cadr op)))
       (jmp (+ pi 1)))
      ((klvm.closure-tail-reg->)
       (tail-fn-> (vm-regs-ref vm (cadr op)) (caddr op))
       (jmp (+ pi 1)))
      ((klvm.closure-tail-fn->)
       (tail-fn-> (vm-fn-ref vm (cadr op)) (caddr op))
       (jmp (+ pi 1)))
      ((klvm.closure-tail-lambda->)
       (tail-fn-> (table-ref (vm-closures vm) (cadr op)) (caddr op))
       (jmp (+ pi 1)))
      ((klvm.push-error-handler)
       (vm-push-error-handler (vm-regs-ref vm (cadr op)) vm)
       (jmp (+ pi 1)))
      ((klvm.pop-error-handler)
       (vm-pop-error-handler vm)
       (jmp (+ pi 1)))
      ((klvm.jump) (jmp (+ pi 1 (cadr op))))
      ((klvm.jmp-unless)
       (jmp (if (vm-regs-ref vm (cadr op))
                (+ pi 1)
                (+ pi 1 (caddr op)))))
      ((klvm.call) (call))
      ((klvm.void-call) (call))
      ((klvm.tail-call) (tail-call))
      ((klvm.ret-reg) (ret-reg (cadr op)))
      ((klvm.ret-fn) (ret-fn (cadr op)))
      ((klvm.ret-const) (ret-const (cadr op)))))
  (jmp* pp pi))

(define (asm-call vm fn . args)
  (define (run expr vm)
    (vm-show-step vm #f "asm-call/run")
    (let ((func (vm-ensure-func (car expr) vm)))
      (set-vm-prev-sp! vm (vm-sp vm))
      (asm-run func vm)))
  (vm-call run vm (cons fn args)))

(define (asm-expr expr)
  (apply asm-call *asm* (car expr) (cdr expr)))

(define (asm.t-x x)
  (clear-log)
  (t1.asm.setup)
  (asm-expr x))

(define (asm.t-do) (asm.t-x '(klvm-test.test-do)))
