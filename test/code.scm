(define current-test #f)
(define test-defs (make-table))

(define (cut-comment s comment-char)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond ((>= i n) s)
            ((char=? (string-ref s i) comment-char) (substring s 0 i))
            (#t (loop (+ i 1)))))))

(define (string-empty? s)
  (define whitespace '(#\space #\tab #\newline #\linefeed))
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond ((>= i n) #t)
            ((member (string-ref s i) whitespace) (loop (+ i 1)))
            (#t #f)))))

(define (parse-src-line line)
  (let ((line (cut-comment line #\;)))
    (with-input-from-string line
      (lambda ()
        (let loop ((acc '()))
          (let ((x (read)))
            (if (eof-object? x)
                (reverse acc)
                (loop (cons x acc)))))))))

(define (parse-asm-src port)
  (define func-tags' (klvm.s1.func klvm.s1.closure klvm.s1.toplevel))
  (define (mk-func hdr const body) `(,@hdr ,(reverse const) ,(reverse body)))
  (let loop ((acc '())
             (hdr '())
             (const '())
             (body '()))
    (let ((line (read-line port)))
      (if (eof-object? line)
          (reverse (if (pair? hdr)
                       (cons (mk-func hdr const body) acc)
                       acc))
          (let ((op (parse-src-line line)))
            (cond ((not (pair? op)) (loop acc hdr const body))
                  ((member (car op) func-tags)
                   (let ((acc (if (pair? hdr)
                                  (cons (mk-func hdr const body) acc)
                                  acc)))
                     (loop acc op '() '())))
                  ((eq? (car op) 'const) (loop acc hdr (cons op const) body))
                  (#t (loop acc hdr const (cons op body)))))))))

(define (read-asm-src-file file)
  (call-with-input-file file parse-asm-src))

(define (test-arg args)
  (if (pair? args)
      (car args)
      current-test))

(define (read-test-defs . ?name)
  (let* ((name (test-arg ?name))
         (defs (with-input-from-file (string-append name ".test") read)))
    (table-set! test-defs name defs)
    defs))

(define (read-test-klvm2 vm . ?name)
  (read-klvm-from-file (string-append (test-arg ?name) ".klvm2") vm))

(define (read-test-asm . ?name)
  (read-klvm-from-file (string-append (test-arg ?name) ".klvma") vm))
