(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex expr)
     (begin
       (write 'expr)
       (write " => ")
       (let ((res expr))
         (write res)
         (newline)
         res)))))
