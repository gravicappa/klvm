(define-record-type klvm
  (mk-klvm* sp sp-top regs nargs next ret err-handlers fns closures)
  klvm?

  ;; Stack pointer.
  (sp klvm-sp set-klvm-sp!)

  ;; Shows the used top of stack. Used for clearing register array.
  (sp-top klvm-sp-top set-klvm-sp-top!) 

  ;; Register array.
  (regs klvm-regs set-klvm-regs!)

  ;; Register to store number of arguments.
  (nargs klvm-nargs set-klvm-nargs!) 

  ;; Pointer to next block.
  (next klvm-next set-klvm-next!) 

  ;; Register to store return value.
  (ret klvm-ret set-klvm-ret!) 

  ;; Stack of error handlers.
  (err-handlers klvm-err-handler set-klvm-err-handlers!)

  ;; Table of functions.
  (fns klvm-fns)

  ;; Table of closures (TODO: store it locally).
  (closures klvm-closures))

(define-record-type klvm-closure
  (mk-klvm-closure name arity code vars)
  klvm-closure?

  ;; Arity of function
  (arity klvm-closure-arity set-klvm-closure-arity!)

  ;; Code which is a vector of lists for interpreted and a procedure for a
  ;; native function
  (code klvm-closure-code set-klvm-closure-code!)

  ;; List of captured variables
  (vars klvm-closure-vars set-klvm-closure-vars!)

  ;; Name of a function if applicable (used basically for printing)
  (name klvm-closure-name set-klvm-closure-name!))

(define-record-type end-marker (mk-end-marker name) end-marker? (name))

(define log-to-repl? #f)
(define log-to-file? #t)
(define log-file "klvm.log")

(define (clear-log) (with-output-to-file log-file (lambda () #f)))

(define (log/* fn x)
  (let ((out (current-output-port)))
    (with-output-to-file `(path: ,log-file append: #t)
      (lambda ()
        (cond (log-to-repl?
               (fn x out)))
        (cond (log-to-file?
               (fn x (current-output-port))))))))

(define (log/pp x)
  (log/* pp x))

(define (log/puts x)
  (log/* (lambda (x port)
           (display x port)
           (newline port))
         x))

(define (klvm-add-func func vm)
  (let ((prev (table-ref (klvm-fns vm) (klvm-closure-name func) #f)))
    (cond ((klvm-closure? prev)
           (set-klvm-closure-arity! prev (klvm-closure-arity func))
           (set-klvm-closure-code! prev (klvm-closure-code func))
           (set-klvm-closure-vars! prev (klvm-closure-vars func))
           (set-klvm-closure-name! prev (klvm-closure-name func))
           prev)
          (#t
           (table-set! (klvm-fns vm) (klvm-closure-name func) func)
           func))))

(define (mk-closure-primitive vm)
  (lambda (func . args)
    (let ((f (ensure-func func vm)))
      (mk-klvm-closure (klvm-closure-name f)
                       (klvm-closure-arity f)
                       (klvm-closure-code f)
                       args))))

(define (klvm-add-native vm name arity func)
  (define (wrap fn)
    (lambda (eval-1 idx)
      (log/pp `(calling native ,name sp: ,(klvm-sp vm)
                nargs: ,(klvm-nargs vm)))
      (let loop ((i 0)
                 (args '()))
        (cond ((< i (klvm-nargs vm))
               (loop (+ i 1)
                     (cons (vector-ref (klvm-regs vm) (+ (klvm-sp vm) i))
                           args)))
              (#t
               (log/pp `(calling native (,name ,@args)))
               (set-klvm-ret!
                vm
                (if (< i arity)
                    (mk-klvm-closure name arity (wrap func) args)
                    (apply func args)))
               (log/pp `((,name ,@args) => ,(klvm-ret vm)))
               (klvm-next vm))))))
  (let ((fn (mk-klvm-closure name arity (wrap func) '())))
    (klvm-add-func fn vm)))

(define (klvm-add-primitive vm name func)
  (define (wrap fn)
    (lambda (eval-1 idx)
      (log/pp `(calling primitive ,name sp: ,(klvm-sp vm)
                nargs: ,(klvm-nargs vm)))
      (let loop ((i 0)
                 (args '()))
        (cond ((< i (klvm-nargs vm))
               (loop (+ i 1)
                     (cons (vector-ref (klvm-regs vm) (+ (klvm-sp vm) i))
                           args)))
              (#t
               (log/pp `(calling primitive (,name ,@args)))
               (set-klvm-ret! vm (apply func args))
               (log/pp `((,name ,@args) => ,(klvm-ret vm)))
               (klvm-next vm))))))
  (let ((fn (mk-klvm-closure name #f (wrap func) '())))
    (klvm-add-func fn vm)))

(define (klvm-init-native vm)
  (klvm-add-primitive vm 'klvm.mk-closure (mk-closure-primitive vm))
  (klvm-add-native vm 'list7 7 list)
  (klvm-add-native vm 'list4 4 list)
  (klvm-add-native vm '= 2 equal?)
  (klvm-add-native vm '> 2 >)
  (klvm-add-native vm '< 2 <)
  (klvm-add-native vm 'hd 1 car)
  (klvm-add-native vm 'tl 1 cdr)
  (klvm-add-native vm 'cons? 1 pair?)
  (klvm-add-native vm '+ 2 +)
  (klvm-add-native vm '- 2 -)
  (klvm-add-native vm '* 2 *)
  (klvm-add-native vm '/ 2 /)
  (klvm-add-native vm 'and 2 (lambda (x y) (and x y)))
  (klvm-add-native vm 'or 2 (lambda (x y) (or x y)))
  (klvm-add-native vm 'cons 2 cons)
  (klvm-add-native vm 'string? 1 string?)
  (klvm-add-native vm 'symbol? 1 symbol?)
  (klvm-add-native vm 'number? 1 number?)
  (klvm-add-native vm 'vector 1 (lambda (n)
                                  (let ((x (make-vector (+ n 1) #f)))
                                    (vector-set! x 0 n)
                                    x)))
  (klvm-add-native vm '<-vector 2 vector-ref)
  (klvm-add-native vm 'vector-> 3 vector-set!)
  )

(define (mk-klvm)
  (let ((vm (mk-klvm* 0 0 (vector) 0 #f #f '() (make-table) (make-table))))
    (klvm-init-native vm)
    vm))

(define *vm* (mk-klvm))

(define (clear-klvm-regs! vm)
  (do ((i 0 (+ i 1)))
      ((>= i (klvm-sp-top vm)))
    (vector-set! (klvm-regs vm) i #f)))

(define (reset-klvm! vm)
  (clear-klvm-regs! vm)
  (set-klvm-sp! vm 0)
  (set-klvm-sp-top! vm 0))

(define (klvm-fn-ref vm name)
  (let ((fn (or (table-ref (klvm-fns vm) name #f)
                (table-ref (klvm-closures vm) name #f))))
    (if fn
        fn
        (error (mkstr name ": no such function")))))

(define (klvm-ensure-regs-size! vm n)
  (let* ((nregs (vector-length (klvm-regs vm)))
         (newsize (+ n (klvm-sp vm))))
    (if (>= newsize nregs)
        (set-klvm-regs! vm (vector-append
                            (klvm-regs vm)
                            (make-vector (- newsize nregs)))))
    (if (>= newsize (klvm-sp-top vm))
        (set-klvm-sp-top! vm newsize))))

(define (number-of-labels labels)
  (let loop ((labels labels)
             (n 0))
    (if (pair? labels)
        (loop (cdr labels) (max n (+ (caar labels) 1)))
        n)))

(define (put-labels-to-vector labels)
  (let ((v (make-vector (number-of-labels labels) '())))
    (let loop ((labels labels))
      (cond ((pair? labels)
             (vector-set! v (caar labels) (cdar labels))
             (loop (cdr labels)))
            (#t v)))))

(define (eval-fn body)
  (let ((vec (put-labels-to-vector (cdr body))))
    (lambda (eval-1 idx) (eval-1 (vector-ref vec idx)))))

(define (mk-func code)
  (apply (lambda (name args nregs body)
           (mk-klvm-closure name (length args) (eval-fn body) '()))
         (cdr code)))

(define (read-klvm code vm)
  (let loop ((code code)
             (toplevel '()))
    (if (pair? code)
        (let ((x (car code)))
          (case (car x)
            ((klvm.func)
             (klvm-add-func (mk-func x) vm)
             (loop (cdr code) toplevel))
            ((klvm.toplevel) (loop (cdr code) (cons (mk-func x) toplevel)))
            ((klvm.closure)
             (let ((f (mk-func x)))
               (table-set! (klvm-closures vm) (klvm-closure-name f) f)
               (loop (cdr code) toplevel)))))
        vm)))

(define (read-klvm-from-file file vm)
  (with-input-from-file
   file
   (lambda ()
     (let loop ((obj (read))
                (code '()))
       (if (eof-object? obj)
           (read-klvm (reverse code) vm)
           (loop (read) (cons obj code)))))))

(define (klvm-entry name nargs)
  ;; Function entry template returned by (klvm.entry-template)
  `((klvm.nargs-cond
     ((klvm.nregs-> (1))
      (klvm.ret-> (klvm.func-obj ,name ,nargs))
      (klvm.wipe-stack)
      (klvm.goto-next))
     ((klvm.nargs- ,nargs))
     ((klvm.sp+ (klvm.nargs))
      (klvm.sp- ,nargs)
      (klvm.nargs- ,nargs)))))

(define (klvm-return x next)
  ;; Function return template returned by (klvm.return-template)
  `((klvm.if-nargs>0
     ((klvm.closure-> ,x)
      (klvm.nregs-> ((klvm.nargs) (klvm.closure-nargs)))
      (klvm.next-> (klvm.reg ,next))
      (klvm.wipe-stack)
      (klvm.put-closure-args 0)
      (klvm.sp- (klvm.nargs))
      (klvm.call))
     ((klvm.ret-> ,x)
      (klvm.next-> (klvm.reg ,next))
      (klvm.wipe-stack)
      (klvm.goto-next)))))

(define (ensure-func func vm)
  (if (symbol? func)
      (klvm-fn-ref vm func)
      func))

(define (klvm-eval func label vm)
  (define closure #f)
  
  (define (eval-2 expr)
    ;(log/pp `(eval-2 ,expr))
    (if (pair? expr)
        (case (car expr)
          ((klvm.reg)
           (vector-ref (klvm-regs vm) (+ (klvm-sp vm) (cadr expr))))
          ((klvm.ret) (klvm-ret vm))
          ((klvm.next) (klvm-next vm))
          ((klvm.nargs) (klvm-nargs vm))
          ((klvm.func-obj)
           (let loop ((i 0)
                      (args '()))
             (if (< i (klvm-nargs vm))
                 (loop (+ i 1)
                       (cons (vector-ref (klvm-regs vm) (+ (klvm-sp vm) i))
                             args))
                 (mk-klvm-closure (cadr expr)
                                  (caddr expr)
                                  (klvm-closure-code func)
                                  (reverse args)))))
          ((klvm.closure-nargs) (length (klvm-closure-vars closure)))
          (else (error `(unexpected expr2 ,expr))))
        (case expr
          ((true) #t)
          ((false) #f)
          (else expr))))
  
  (define (eval-1 exprs)
    (if (pair? exprs)
        (let ((expr (car exprs)))
          (log/pp `(eval-1 ,expr))
          (case (car expr)
            ((klvm.entry)
             (eval-1 (append (klvm-entry (cadr expr) (caddr expr))
                             (cdr exprs))))
            ((klvm.return)
             (eval-1 (append (klvm-return (cadr expr) (caddr expr))
                             (cdr expr))))
            ((klvm.goto) (cons func (cadr expr)))
            ((klvm.call) (cons closure 0))
            ((klvm.goto-next) (klvm-next vm))
            ((klvm.if)
             (if (eval-2 (cadr expr))
                 (eval-1 (cons (caddr expr) (cdr exprs)))
                 (eval-1 (cons (cadddr expr) (cdr exprs)))))
            ((klvm.nargs-cond)
             (log/pp `(nargs-cond
                       nargs: ,(klvm-nargs vm)
                       arity: ,(klvm-closure-arity func)
                       func: ,(klvm-closure-name func)))
             (cond ((< (klvm-nargs vm) (klvm-closure-arity func))
                    (eval-1 (append (cadr expr) (cdr exprs))))
                   ((= (klvm-nargs vm) (klvm-closure-arity func))
                    (eval-1 (append (caddr expr) (cdr exprs))))
                   ((> (klvm-nargs vm) (klvm-closure-arity func))
                    (eval-1 (append (cadddr expr) (cdr exprs))))))
            ((klvm.if-nargs>0)
             (if (> (klvm-nargs vm) 0)
                 (eval-1 (append (cadr expr) (cdr exprs)))
                 (eval-1 (append (caddr expr) (cdr exprs)))))
            (else
             (case (car expr)
               ((klvm.put-closure-args)
                (let ((n (cadr expr))
                      (nargs (length (klvm-closure-vars closure))))
                  (klvm-ensure-regs-size! vm (+ nargs n))
                  (let loop ((args (klvm-closure-vars closure))
                             (i (+ (klvm-sp vm) n)))
                    (cond ((pair? args)
                           (vector-set! (klvm-regs vm) i (car args))
                           (loop (cdr args) (+ i 1)))
                          (#t
                           (set-klvm-nargs! vm (+ (klvm-nargs vm) nargs)))))))
               
               ((klvm.ret->) (set-klvm-ret! vm (eval-2 (cadr expr))))
               ((klvm.nregs->)
                (klvm-ensure-regs-size! vm (apply + (map eval-2
                                                         (cadr expr)))))
               ((klvm.reg->)
                (vector-set! (klvm-regs vm)
                             (+ (klvm-sp vm) (cadr expr))
                             (eval-2 (caddr expr))))
               ((klvm.next->)
                (set-klvm-next! vm (if (number? (cadr expr))
                                       (cons func (cadr expr))
                                       (eval-2 (cadr expr)))))
               ((klvm.sp+)
                (set-klvm-sp! vm (+ (klvm-sp vm) (eval-2 (cadr expr)))))
               ((klvm.sp-)
                (set-klvm-sp! vm (- (klvm-sp vm) (eval-2 (cadr expr)))))
               ((klvm.nargs->) (set-klvm-nargs! vm (eval-2 (cadr expr))))
               ((klvm.nargs+)
                (set-klvm-nargs! vm (+ (klvm-nargs vm) (eval-2 (cadr expr)))))
               ((klvm.nargs-)
                (set-klvm-nargs! vm (- (klvm-nargs vm) (eval-2 (cadr expr)))))
               ((klvm.closure->)
                (set! closure (ensure-func (eval-2 (cadr expr)) vm)))
               ((klvm.wipe-stack) #t)
               (else (error `(unexpected ,expr))))
             (eval-1 (cdr exprs)))))
        #f))
  
  ((klvm-closure-code func) eval-1 label))

(define (show-step vm pc)
  (define (str-pc pc)
    (if (and (pair? pc)
             (klvm-closure? (car pc))
             (klvm-closure-name (car pc)))
        (cons (klvm-closure-name (car pc)) (cdr pc))
        pc))
  (log/puts "")
  (log/puts "## STEP")
  (log/pp `(cur: ,(str-pc pc)))
  (log/pp `(nargs: ,(klvm-nargs vm)))
  (log/pp `(sp: ,(klvm-sp vm)))
  (log/pp `(ret: ,(klvm-ret vm)))
  (log/pp `(next: ,(str-pc (klvm-next vm))))
  (log/pp `(regs: ,(if #t
                       (klvm-regs vm)
                       (subvector (klvm-regs vm)
                                  (klvm-sp vm)
                                  (vector-length (klvm-regs vm)))))))

(define (klvm-run func label vm)
  (let loop ((next (cons func label)))
    (show-step vm next)
    (cond ((pair? next) (loop (klvm-eval (car next) (cdr next) vm)))
          ((not next) #t)
          ((tag? next) #t)
          (#t (log/pp `(exception: ,next))))))

(define (klvm-put-args args vm)
  (let loop ((args args)
             (i (klvm-sp vm)))
    (cond ((pair? args)
           (vector-set! (klvm-regs vm) i (car args))
           (loop (cdr args) (+ i 1)))
          (#t i))))

(define (klvm-call vm fn . args)
  (let ((fn (if (klvm-closure? fn)
                fn
                (klvm-fn-ref vm fn)))
        (nargs (length args))
        (old-sp (klvm-sp vm)))
    (set-klvm-sp-top! vm (klvm-sp vm))
    (set-klvm-next! vm (mk-end-marker "END"))
    (set-klvm-ret! vm #f)
    (set-klvm-nargs! vm (+ nargs (length (klvm-closure-vars fn))))
    (klvm-ensure-regs-size! vm (klvm-nargs vm))
    (klvm-put-args (append (reverse args) (klvm-closure-vars fn)) vm)
    (klvm-run fn 0 vm)
    (if (not (= old-sp (klvm-sp vm)))
        (log/pp `(SP IS BROKEN: ,(klvm-sp vm) (expected: ,old-sp))))
    (klvm-ret vm)))

(define (t1.setup)
  (reset-klvm! *vm*)
  (read-klvm-from-file "code.klvm2" *vm*))

(define (t1)
  (t1.setup)
  (test '(
          (klvm-test.list-len ()) => 0
          (klvm-test.list-len (1 2 3 4 a)) => 5
          (klvm-test.test-call 9) => 23
          (klvm-test.reversex ()) => ()
          (klvm-test.reversex (1 2 3 4 5)) => (5 4 3 2 1)
          (+ 7 8) => 15
          (- 7 8) => -1
          (klvm-test.test-partial) => 9
          (klvm-test.test-closure) => 14
          (klvm-test.test-closure-2) => (1 2 3 4 5)
          (klvm-test.test-closure-3) => (1 2 3 4 5 klvm-test.a klvm-test.b)
          (klvm-test.test-freeze) => 25
          (klvm-test.test-closure-4) => 17
          (klvm-test.test-closure-5) => 0
          (klvm-test.test-map ()) => ()
          (klvm-test.test-map (-1 2 -3 4 -5 6)) => (#f #t #f #t #f #t)
          (klvm-test.incr-list (-1 2 -3 4 -5 6)) => (0 3 -2 5 -4 7)
          (klvm-test.incr-list-aux (-1 2 -3 4 -5 6)) => (0 3 -2 5 -4 7)
          (klvm-test.test-guard) => ((klvm-test.num 1)
                                     (klvm-test.sym klvm-test.two)
                                     (str "three")
                                     (str "four")
                                     (klvm-test.num 5)
                                     (klvm-test.sym klvm-test.six))
          (klvm-test.test-do) => #(3 3 5 15)
          (klvm-test.test-and #t #t) => #t
          (klvm-test.test-and #t #f) => #f
          (klvm-test.test-and #f #t) => #f
          (klvm-test.test-and #f #f) => #f
          (klvm-test.test-and-2 #t #t) => #t
          (klvm-test.test-and-2 #t #f) => #f
          (klvm-test.test-and-2 #f #t) => #f
          (klvm-test.test-and-2 #f #f) => #f
          (klvm-test.test-or #t #t) => #t
          (klvm-test.test-or #f #t) => #t
          (klvm-test.test-or #t #f) => #t
          (klvm-test.test-or #f #f) => #f
          (klvm-test.test-or-2 #t #t) => #t
          (klvm-test.test-or-2 #f #t) => #t
          (klvm-test.test-or-2 #t #f) => #t
          (klvm-test.test-or-2 #f #f) => #f
          (klvm-test.appendx (a b c) (2 3 4)) => (a b c 2 3 4)
          (klvm-test.test-let-1 #t) => #(4 3 5 15 klvm-test.one)
          (klvm-test.test-let-1 #f) => #(4 0 1 2 klvm-test.one)
          (klvm-test.test-freeze-2) => 80
          (klvm-test.test-trap-error) => 0
          (klvm-test.test-trap-error-aux) => 0
        )))
