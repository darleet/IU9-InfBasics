(define memoized-factorial
  (let ((memo-res '()))
    (lambda (n)
      (let* ((step-res (assoc n memo-res)))
        (if step-res
            (cadr step-res)
            (let ((new-res
                   (cond ((= n 0) 1)
                         (else (* n (memoized-factorial (- n 1)))))))
              (set! memo-res (cons (list n new-res) memo-res))
              new-res))))))