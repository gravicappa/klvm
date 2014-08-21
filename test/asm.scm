(define *asm* (mk-vm))

(define-record-type asm-code
  (mk-asm-code code nregs frame-size)
  asm-code?
  (code asm-code-code)
  (nregs asm-code-nregs)
  (frame-size asm-code-frame-size))

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

(define (asm-closure-nregs fn)
  (asm-code-nregs (vm-closure-code fn)))

(define (asm-closure-frame-size fn)
  (asm-code-frame-size (vm-closure-code fn)))

(define (asm-next-reg fn vm)
  (- (asm-closure-nregs fn) 1))

(define (asm-nargs-reg fn vm)
  (- (asm-closure-nregs fn) 2))

(define (asm-prev-sp-reg fn vm)
  (- (asm-closure-nregs fn) 3))

(define (asm-func-entry fn vm)
  (log/pp `(asm-func-entry: ,fn))
  (cond ((asm-code? (vm-closure-code fn))
         (let ((nargs (vm-nargs vm))
               (arity (vm-closure-arity fn)))
           (log/pp `(arity: ,arity nargs: ,nargs))
           (cond ((< nargs arity)
                  (set-vm-ret! vm (vm-func-obj (vm-closure-name fn)
                                               arity
                                               (vm-closure-code fn)
                                               vm))
                  #t)
                 ((> nargs arity)
                  (set-vm-sp! vm (+ (vm-sp vm) (- nargs arity)))
                  (set-vm-nargs! vm (- nargs arity))
                  (vm-nregs-> vm (asm-closure-frame-size fn))
                  (vm-regs-set! vm (asm-next-reg fn vm) (vm-next vm))
                  (vm-regs-set! vm (asm-nargs-reg fn vm) (vm-nargs vm))
                  (vm-regs-set! vm (asm-prev-sp-reg fn vm) (vm-prev-sp vm))
                  #f)
                 (#t
                  (set-vm-nargs! vm (- nargs arity))
                  (vm-nregs-> vm (asm-closure-frame-size fn))
                  (log/pp `(entry sp: ,(vm-sp vm)
                            nargs-reg: ,(asm-nargs-reg fn vm)))
                  (vm-regs-set! vm (asm-next-reg fn vm) (vm-next vm))
                  (vm-regs-set! vm (asm-nargs-reg fn vm) (vm-nargs vm))
                  (vm-regs-set! vm (asm-prev-sp-reg fn vm) (vm-prev-sp vm))
                  #f))))
        ((procedure? (vm-closure-code fn)) #f)))

(define (asm-run pc vm)
  (define current-fn (car pc))
  (define closure #f)

  (define (call* next pc)
    (set-vm-nargs! vm (+ (vm-nargs vm) (length (vm-closure-vars closure))))
    (vm-nregs-> vm (vm-nargs vm))
    (vm-put-args (vm-closure-vars closure) vm)
    (cond ((procedure? (vm-closure-code closure))
           ((vm-closure-code closure) #f #f)
           (set-vm-sp! vm (vm-prev-sp vm))
           (next (car (vm-next vm)) (cdr (vm-next vm))))
          ((asm-func-entry closure vm)
           (next (car (vm-next vm)) (cdr (vm-next vm))))
          (#t
           (set! current-fn closure)
           (next closure 0)
           (set-vm-sp! vm (vm-prev-sp vm)))))

  (define (call next pc)
    (set-vm-next! vm pc)
    (set-vm-prev-sp! vm (vm-sp vm))
    (set-vm-sp! vm (+ (vm-sp vm) (asm-closure-nregs current-fn)))
    (call* next pc))

  (define (tail-call next pc)
    (set-vm-prev-sp! vm (vm-regs-ref vm (asm-prev-sp-reg current-fn vm)))
    (set-vm-next! vm (vm-regs-ref vm (asm-next-reg current-fn vm)))
    (call* next pc))

  (define (ret x next-reg next)
    (log/pp `(ret current-fn: ,current-fn
              sp: ,(vm-sp vm)
              next-reg: ,next-reg
              nargs-reg: ,(asm-nargs-reg current-fn vm)))
    (set-vm-nargs! vm (vm-regs-ref vm (asm-nargs-reg current-fn vm)))
    (cond ((positive? (vm-nargs vm))
           (set-vm-next! vm (vm-regs-ref vm next-reg))
           (vm-put-args (vm-closure-vars x) vm)
           (set-vm-prev-sp! vm
                            (vm-regs-ref vm (asm-prev-sp-reg current-fn vm)))
           (set-vm-sp! vm (- (vm-sp vm) (vm-nargs vm)))
           (next x 0))
          (#t
           (let ((pc (vm-regs-ref vm next-reg)))
             (set-vm-ret! vm x)
             (set-vm-sp! vm (vm-prev-sp vm))
             (next (car pc) (cdr pc))))))

  (define (ret-reg reg next-reg next)
    (ret (vm-regs-ref vm reg) next-reg next))

  (define (ret-fn x next)
    (ret (table-ref (vm-closures vm) x) next-reg next))

  (define (ret-const x next)
    (ret x next-reg next))

  (let loop ((pc0 (car pc))
             (pc1 (cdr pc)))
    (vm-show-step vm (cons pc0 pc1) "STEP")
    (if (and pc0 pc1)
        (let ((x (vector-ref (asm-closure-code pc0) pc1)))
          (log/pp `(op: ,x))
          (case (car x)
            ((klvm.load-reg->)
             (vm-regs-set! vm (cadr x) (vm-regs-ref vm (caddr x)))
             (loop pc0 (+ pc1 1)))
            ((klvm.ret->)
             (set-vm-ret! vm (vm-regs-ref vm (cadr x)))
             (loop pc0 (+ pc1 1)))
            ((klvm.load-fn->)
             (vm-regs-set! vm
                           (cadr x)
                           (table-ref (vm-closures vm) (caddr expr)))
             (loop pc0 (+ pc1 1)))
            ((klvm.load-const->)
             (vm-regs-set! vm (cadr x) (caddr x))
             (loop pc0 (+ pc1 1)))
            ((klvm.closure-reg->)
             (set! closure (vm-regs-ref vm (cadr x)))
             (set-vm-nargs! vm (caddr x))
             (loop pc0 (+ pc1 1)))
            ((klvm.closure-fn->)
             (set! closure (vm-fn-ref vm (cadr x)))
             (set-vm-nargs! vm (caddr x))
             (loop pc0 (+ pc1 1)))
            ((klvm.closure-lambda->)
             (set-vm-nargs! vm (caddr x))
             (set! closure (table-ref (vm-closures vm) (cadr x)))
             (loop pc0 (+ pc1 1)))
            ((klvm.push-error-handler)
             (vm-push-error-handler (vm-regs-ref vm (cadr expr)) vm)
             (loop pc0 (+ pc1 1)))
            ((klvm.pop-error-handler)
             (vm-pop-error-handler vm)
             (loop pc0 (+ pc1 1)))
            ((klvm.jump) (loop pc0 (+ pc1 1 (cadr x))))
            ((klvm.jmp-unless)
             (loop pc0 (if (vm-regs-ref vm (cadr x))
                           (+ pc1 1)
                           (+ pc1 1 (caddr x)))))
            ((klvm.call) (call loop (cons pc0 (+ pc1 1))))
            ((klvm.void-call) (void-call loop (cons pc0 (+ pc1 1))))
            ((klvm.tail-call) (tail-call loop (cons pc0 (+ pc1 1))))
            ((klvm.ret-reg) (ret-reg (cadr x) (caddr x) loop))
            ((klvm.ret-fn) (ret-fn (cadr x) (caddr x) loop))
            ((klvm.ret-const) (ret-const (cadr x) (caddr x) loop)))))))

(define (asm-call vm fn . args)
  (define (run expr vm)
    (let ((func (vm-ensure-func (car expr) vm)))
      (set-vm-prev-sp! vm (vm-sp vm))
      (asm-func-entry func vm)
      (asm-run (cons func 0) vm)))
  (vm-call run vm (cons fn args)))

(define (asm-expr expr)
  (apply asm-call *asm* (car expr) (cdr expr)))