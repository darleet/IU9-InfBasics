(define-syntax test
  (syntax-rules ()
    ((_ expr expected-result)
     (list (quote expr) expected-result)))) ; Преобразование в list

(define ie (interaction-environment))

(define (run-tests tests)
  (define (loop correct? tests)
    (if (null? tests)
        correct?
        (let ((expr (caar tests)))
          (write expr)
          (let* ((result (eval expr ie))
                 (check (equal? result (cadar tests))))
            (if check
                (begin
                  (display " ok") (newline))
                (begin
                  (display " FAIL") (newline)
                  (display "  Expected: ") (write (cadar tests)) (newline)
                  (display "  Returned: ") (write result) (newline)))
            (loop (and correct? check) (cdr tests))))))
  (loop #t tests))

(define (run-test test)
  (run-tests (list test)))