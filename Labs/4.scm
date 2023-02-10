;---1---

(define call/cc call-with-current-continuation)
(define exit #f)

(define (use-assertions)
  (call/cc
   (lambda (c)
     (set! exit c))))

(define-syntax assert
  (syntax-rules ()
    ((_ expr)
     (if (not expr)
         (begin
           (display "FAILED: ")
           (write (quote expr))
           (newline)
           (exit))))))

(use-assertions)

(define (1/x x)
  (assert (not (zero? x)))
  (write (/ 1 x))
  (newline))

;---2---

(define (save-data var path)
  (call-with-output-file path
    (lambda (port)
      (write var port)
      (newline port))))

(define (load-data path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((data (read port)))
        (if (not (eof-object? data))
            (begin (write data) (loop (read port)))
            (newline))))))

(define (line-count path)
  (call-with-input-file path
    (lambda (port)
      (define (loop counter prev-newline?)
        (let ((symbol (read-char port)))
          (cond ((and (equal? symbol #\newline) (not prev-newline?))
                 (loop (+ 1 counter) (equal? symbol #\newline)))
                ((eof-object? symbol) counter)
                (else (loop counter (equal? symbol #\newline))))))
      (loop 0 #\newline))))

;---3---

(define trib-memo
  (let ((memo-res '()))
    (lambda (n)
      (let* ((step-res (assoc n memo-res)))
        (if step-res
            (cadr step-res)
            (let ((new-res
                   (cond ((<= n 1) 0)
                         ((= n 2) 1)
                         (else (+ (trib-memo (- n 3))
                                  (trib-memo (- n 2))
                                  (trib-memo (- n 1)))))))
              (set! memo-res (cons (list n new-res) memo-res))
              new-res))))))

;---4---

(define-syntax my-if
  (syntax-rules ()
    ((_ predicate then else)
     (let ((a (delay then))
           (b (delay else)))
       (or (and predicate (force a)) (force b))))))


;---5---

(define-syntax my-let
  (syntax-rules ()
    ((_ ((var value) ...) body ...)
     ((lambda (var ...) body ...) value ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () body ...)
     (my-let () body ...))
    ((my-let* ((var value)) body ...)
     (my-let ((var value)) body ...))
    ((my-let* ((var value) . other) body ...)
     ((lambda (var) (my-let* other body ...)) value))))


;---6---

(define-syntax when
  (syntax-rules ()
    ((_ cond? body ...)
     (if cond? (begin body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ cond? body ...)
     (if (not cond?) (begin body ...)))))

(define-syntax for
  (syntax-rules (in as)
    ((_ i in xs body ...)
     (let loop ((vals xs))
       (if (not (null? vals))
           (let ((i (car vals)))
             body ...
             (loop (cdr vals))))))
    ((_ xs as i body ...)
     (for i in xs body ...))))

(define-syntax while
  (syntax-rules ()
    ((while cond? body ...)
     (let loop ()
       (if cond? (begin body ... (loop)))))))

(define-syntax repeat
  (syntax-rules (until)
    ((repeat (body ...) until cond?)
     (let loop ()
       body ...
       (if (not cond?) (loop))))))

(define-syntax cout
  (syntax-rules (<< endl)
    ((_ << endl)
     (newline))
    ((_ << endl . expr)
     (begin (newline)
            (cout . expr)))
    ((_ << expr1)
     (display expr1))
    ((_ << expr1 . expr)
     (begin (display expr1)
            (cout . expr)))))