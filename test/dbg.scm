(define log-to-repl? #f)
(define log-file "klvm.log")

(define (clear-log) (with-output-to-file log-file (lambda () #f)))

(define (log/* fn x)
  (if log-to-repl?
      (fn x (current-output-port)))
  (if (string? log-file)
      (call-with-output-file `(path: ,log-file append: #t)
                             (lambda (port) (fn x port)))))

(define (log/pp x)
  (log/* pp x))

(define (log/puts x)
  (log/* (lambda (x port)
           (display x port)
           (newline port))
         x))

(define (err/pp x)
  (log/pp x)
  (pp x))
