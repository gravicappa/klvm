(define-record-type vm
  (mk-vm* sp sp-top regs nargs next ret err-handlers fns closures)
  vm?

  ;; Stack pointer.
  (sp vm-sp set-vm-sp!)

  ;; Shows the used top of stack. Used for clearing register array.
  (sp-top vm-sp-top set-vm-sp-top!) 

  ;; Register array.
  (regs vm-regs set-vm-regs!)

  ;; Register to store number of arguments.
  (nargs vm-nargs set-vm-nargs!) 

  ;; Pointer to next block.
  (next vm-next set-vm-next!) 

  ;; Register to store return value.
  (ret vm-ret set-vm-ret!) 

  ;; Register to store prev-sp (used exclusively in bytecode/asm
  ;; interpreter).
  (prev-sp vm-prev-sp set-vm-prev-sp!)

  ;; Stack of error handlers.
  (err-handlers vm-err-handlers set-vm-err-handlers!)

  ;; Table of functions.
  (fns vm-fns)

  ;; Table of closures (we can store it locally for each input file and
  ;; insert directly into code).
  (closures vm-closures))

(define-record-type vm-closure
  (mk-vm-closure name arity code vars)
  vm-closure?

  ;; Arity of function
  (arity vm-closure-arity set-vm-closure-arity!)

  ;; Code which is a procedure for a native function and something else
  ;; for interpreted
  (code vm-closure-code set-vm-closure-code!)

  ;; List of captured variables
  (vars vm-closure-vars set-vm-closure-vars!)

  ;; Name of a function if applicable (used basically for printing)
  (name vm-closure-name set-vm-closure-name!))

(define-record-type vm-error-handler
  (mk-vm-error-handler func sp next)
  vm-error-handler?
  (func vm-error-handler-func)
  (sp vm-error-handler-sp)
  (next vm-error-handler-next))

(define-record-type vm-end-marker
  (mk-vm-end-marker name)
  vm-end-marker?
  (name))

(define (mkstr . args)
  (with-output-to-string '()
    (lambda () (for-each display args))))

(define (strjoin lst sep)
  (let loop ((lst lst)
             (ret "")
             (s ""))
    (if (pair? lst)
        (loop (cdr lst) (mkstr ret s (car lst)) sep)
        ret)))

(define (vm-regs-ref vm reg)
  (vector-ref (vm-regs vm) (+ (vm-sp vm) reg)))

(define (vm-regs-set! vm reg x)
  (vector-set! (vm-regs vm) (+ (vm-sp vm) reg) x))

(define (vm-add-func! func vm)
  (let ((prev (table-ref (vm-fns vm) (vm-closure-name func) #f)))
    (cond ((vm-closure? prev)
           (set-vm-closure-arity! prev (vm-closure-arity func))
           (set-vm-closure-code! prev (vm-closure-code func))
           (set-vm-closure-vars! prev (vm-closure-vars func))
           (set-vm-closure-name! prev (vm-closure-name func))
           prev)
          (#t
           (table-set! (vm-fns vm) (vm-closure-name func) func)
           func))))

(define (vm-push-error-handler handler vm)
  (let ((x (mk-vm-error-handler handler (vm-sp vm) (vm-next vm))))
    (set-vm-err-handlers! vm (cons x (vm-err-handlers vm)))
    #f))

(define (vm-pop-error-handler vm)
  (if (pair? (vm-err-handlers vm))
      (let ((e (car (vm-err-handlers vm))))
        (set-vm-err-handlers! vm (cdr (vm-err-handlers vm)))
        e)
      #f))

(define (vm-ensure-func func vm)
  (cond ((symbol? func) (vm-fn-ref vm func))
        ((vm-closure? func) func)
        (#t (error `(is not a function ,func)))))

(define (vm-closure-primitive vm)
  (lambda (func . args)
    (log/pp `(vm-closure-primitive))
    (let ((f (vm-ensure-func func vm)))
      (mk-vm-closure (vm-closure-name f)
                     (vm-closure-arity f)
                     (vm-closure-code f)
                     args))))

(define (vm-add-native! vm name arity func)
  (define (wrap fn)
    (lambda (eval-1 _)
      (log/pp `(calling native ,name sp: ,(vm-sp vm)
                nargs: ,(vm-nargs vm)
                arity: ,arity))
      (let loop ((i 0)
                 (args '()))
        (cond ((< i (vm-nargs vm))
               (loop (+ i 1) (cons (vm-regs-ref vm i) args)))
              (#t
               (set-vm-sp-top! vm (+ (vm-sp vm) i))
               (log/pp `(calling native (,name ,@args)))
               (cond ((< i arity)
                      (set-vm-ret!
                       vm (mk-vm-closure name arity (wrap func) args))
                      (log/pp `(native sp-top: ,(vm-sp-top vm)))
                      (vm-wipe vm 0)
                      (log/pp `((,name ,@args) => ,(vm-ret vm)))
                      (vm-next vm))
                     (#t
                      (with-exception-catcher
                        (lambda (e)
                          (vm-show-step vm #f "EXCEPTION")
                          (log/pp `((,name ,@args) => ,e))
                          e)
                        (lambda ()
                          (set-vm-ret! vm (apply func args))
                          (vm-wipe vm 0)
                          (log/pp `((,name ,@args) => ,(vm-ret vm)))
                          (vm-next vm))))))))))
  (let ((fn (mk-vm-closure name arity (wrap func) '())))
    (vm-add-func! fn vm)))

(define (vm-add-primitive! vm name func)
  (define (wrap fn)
    (lambda (eval-1 _)
      (log/pp `(calling primitive ,name sp: ,(vm-sp vm)
                nargs: ,(vm-nargs vm)))
      (let loop ((i 0)
                 (args '()))
        (cond ((< i (vm-nargs vm))
               (loop (+ i 1) (cons (vm-regs-ref vm i) args)))
              (#t
               (log/pp `(calling primitive (,name ,@args)))
               (set-vm-sp-top! vm (+ (vm-sp vm) i))
               (vm-wipe vm 0)
               (set-vm-ret! vm (apply func args))
               (log/pp `((,name ,@args) => ,(vm-ret vm)))
               (vm-next vm))))))
  (let ((fn (mk-vm-closure name #f (wrap func) '())))
    (vm-add-func! fn vm)))

(define (vm-init-native vm)
  (vm-add-primitive! vm 'klvm.mk-closure (vm-closure-primitive vm))
  (vm-add-native! vm 'list7 7 list)
  (vm-add-native! vm 'list4 4 list)
  (vm-add-native! vm '= 2 equal?)
  (vm-add-native! vm '> 2 >)
  (vm-add-native! vm '< 2 <)
  (vm-add-native! vm 'hd 1 car)
  (vm-add-native! vm 'tl 1 cdr)
  (vm-add-native! vm 'cons? 1 pair?)
  (vm-add-native! vm '+ 2 +)
  (vm-add-native! vm '- 2 -)
  (vm-add-native! vm '* 2 *)
  (vm-add-native! vm '/ 2 /)
  (vm-add-native! vm 'and 2 (lambda (x y) (and x y)))
  (vm-add-native! vm 'or 2 (lambda (x y) (or x y)))
  (vm-add-native! vm 'cons 2 cons)
  (vm-add-native! vm 'string? 1 string?)
  (vm-add-native! vm 'symbol? 1 symbol?)
  (vm-add-native! vm 'number? 1 number?)
  (vm-add-native! vm 'vector 1 (lambda (n)
                                  (let ((x (make-vector (+ n 1) #f)))
                                    (vector-set! x 0 n)
                                    x)))
  (vm-add-native! vm '<-vector 2 vector-ref)
  (vm-add-native! vm 'vector-> 3 vector-set!))

(define (mk-vm)
  (let ((vm (mk-vm* 0 0 (vector) 0 #f #f '() (make-table) (make-table))))
    (vm-init-native vm)
    vm))

(define (clear-vm-regs! vm)
  (do ((i 0 (+ i 1)))
      ((>= i (vector-length (vm-regs vm))))
    (vector-set! (vm-regs vm) i #f)))

(define (reset-vm! vm)
  (clear-vm-regs! vm)
  (set-vm-sp! vm 0)
  (set-vm-prev-sp! vm 0)
  (set-vm-sp-top! vm 0))

(define (vm-fn-ref vm name)
  (let ((fn (table-ref (vm-fns vm) name #f)))
    (if fn
        fn
        (error (mkstr name ": no such function")))))

(define (vm-ensure-stack-size! vm size)
  (let* ((nregs (vector-length (vm-regs vm))))
    (if (>= size nregs)
        (set-vm-regs! vm (vector-append
                            (vm-regs vm)
                            (make-vector (- size nregs)))))))

(define (vm-wipe-2 vm start)
  (let ((end (vm-sp-top vm))
        (start (+ (vm-sp vm) start)))
    (log/pp `(wipe end: ,end))
    (do ((i start (+ i 1)))
        ((>= i end))
      (vector-set! (vm-regs vm) i #f))
    (set-vm-sp-top! vm start)))

(define (vm-wipe vm start)
  (define log `(wipe sp: ,(vm-sp vm) start: ,start
                regs-len: ,(vector-length (vm-regs vm))
                regs: ,(subvector (vm-regs vm)
                                  (vm-sp vm)
                                  (vector-length (vm-regs vm)))))
  (vm-wipe-2 vm start)
  (log/pp `(,@log after: ,(subvector (vm-regs vm)
                                     (vm-sp vm)
                                     (vector-length (vm-regs vm))))))

(define (vm-put-args args off vm)
  (let loop ((args args)
             (i off))
    (cond ((pair? args)
           (vector-set! (vm-regs vm) i (car args))
           (loop (cdr args) (+ i 1)))
          (#t i))))

(define (vm-nregs-> vm n)
  (let ((newsize (+ (vm-sp vm) n)))
    (vm-ensure-stack-size! vm newsize)
    (set-vm-sp-top! vm newsize)))

(define (vm-call* fn args vm)
  (let ((fn (vm-ensure-func fn vm))
        (nargs (length args)))
    (set-vm-ret! vm #f)
    (set-vm-nargs! vm (+ nargs (length (vm-closure-vars fn))))
    (vm-nregs-> vm (vm-nargs vm))
    (vm-put-args (append (reverse args) (vm-closure-vars fn)) (vm-sp vm) vm)))

(define (vm-call run vm expr)
  (let ((nargs (length (cdr expr)))
        (old-sp (vm-sp vm)))
    (set-vm-sp-top! vm (vm-sp vm))
    (set-vm-next! vm (mk-vm-end-marker "END"))
    (vm-call* (car expr) (cdr expr) vm)
    (run expr vm)
    (vm-wipe vm 0)
    (cond ((not (= old-sp (vm-sp vm)))
           (err/pp `(SP IS BROKEN: ,(vm-sp vm) (expected: ,old-sp)))
           (error 'sp-broken)))
    (if (not (vm-stack-empty? vm))
        (error 'stack-not-empty))
    (vm-ret vm)))

(define (vm-stack-empty? vm)
  (let* ((regs (vm-regs vm))
         (n (vector-length regs)))
    (let loop ((i (vm-sp vm))
               (ret #t))
      (cond ((>= i n) ret)
            ((vector-ref regs i)
             (err/pp `(STACK IS NOT EMPTY i: ,i
                       regs: , (subvector regs (vm-sp vm) n)))
             #f)
            (#t (loop (+ i 1) ret))))))

(define (vm-func-obj name arity code vm)
  (let loop ((i 0)
             (args '()))
    (if (< i (vm-nargs vm))
        (loop (+ i 1) (cons (vm-regs-ref vm i) args))
        (mk-vm-closure name arity code (reverse args)))))

(define (vm-show-step vm pc title)
  (define (str-pc pc)
    (if (and (pair? pc)
             (vm-closure? (car pc))
             (vm-closure-name (car pc)))
        (cons (vm-closure-name (car pc)) (cdr pc))
        pc))
  (cond (log-step?
         (log/puts "")
         (log/puts (mkstr "## " title))
         (log/pp `(cur: ,(str-pc pc)))
         (log/pp `(nargs: ,(vm-nargs vm)))
         (log/pp `(sp: ,(vm-sp vm)))
         (log/pp `(prev-sp: ,(vm-prev-sp vm)))
         (log/pp `(sp-top: ,(vm-sp-top vm)))
         (log/pp `(ret: ,(vm-ret vm)))
         (log/pp `(next: ,(str-pc (vm-next vm))))
         (log/pp `(regs: ,(if #t
                              (vm-regs vm)
                              (subvector (vm-regs vm)
                                         (vm-sp vm)
                                         (vector-length (vm-regs vm)))))))))
