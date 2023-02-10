(define (o . procs)
  (define (loop procs)
    (if (null? procs)
        (lambda (x) x)
        (let ((proc (car procs)))
          (if (null? (cdr procs))
              proc
              (lambda (x) (proc ((loop (cdr procs)) x)))))))
  (loop procs))