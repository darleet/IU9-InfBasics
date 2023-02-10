(define-syntax define-struct
  (syntax-rules ()
    ((_ symbol-name symbol-fields)
     (let ((name (symbol->string 'symbol-name))
           (fields (map symbol->string 'symbol-fields)))
       (define (loop fields counter)
         (if (not (null? fields))
             (begin
               (eval `(begin
                        (define (,(string->symbol
                                   (string-append name
                                                  "-"
                                                  (car fields)))
                                 object)
                          (vector-ref object ,counter))
                        (define (,(string->symbol
                                   (string-append "set-"
                                                  name
                                                  "-"
                                                  (car fields)
                                                  "!"))
                                 object
                                 value)
                          (vector-set! object ,counter value)))
                     (interaction-environment))
               (loop (cdr fields) (+ 1 counter)))))
       (loop fields 1)
       (eval `(begin
                (define (,(string->symbol
                           (string-append "make-" name))
                         . values)
                  (list->vector (cons 'symbol-name values)))
                (define (,(string->symbol
                           (string-append name "?"))
                         object)
                  (and (vector? object)
                       (> (vector-length object) 0)
                       (equal? (vector-ref object 0) 'symbol-name))))
             (interaction-environment))))))