(define-syntax define-data
  (syntax-rules ()
    ((_ sym-name sym-constructors)
     (let ((name (symbol->string 'sym-name))
           (constructors 'sym-constructors))
       (define (loop constructors)
         (if (not (null? constructors))
             (begin
               (eval `(define (,(caar constructors)
                               . params)
                        (cons 'sym-name
                              (cons ',(caar constructors)
                                    params)))
                     (interaction-environment))
               (loop (cdr constructors)))))
       (loop constructors)
       (eval `(define (,(string->symbol
                         (string-append name "?"))
                       object)
                (and (list? object)
                     (not (null? object))
                     (equal? 'sym-name (car object))))
             (interaction-environment))))))

(define-syntax match
  (syntax-rules ()
    ((_ argument ((name params ...) expr))
     (apply (lambda (params ...) expr) (cddr argument)))
    ((_ argument ((name params ...) expr) samples ...)
     (if (equal? 'name (cadr argument))
         (apply (lambda (params ...) expr) (cddr argument))
         (match argument samples ...)))))
         


      

(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))

(define s (square 10))
(define r (rectangle 10 20))
(define t (triangle 10 20 30))
(define c (circle 10))

(and (figure? s)
     (figure? r)
     (figure? t)
     (figure? c))

(define pi (acos -1))
  
(define (perim f)
  (match f 
    ((square a)       (* 4 a))
    ((rectangle a b)  (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r)       (* 2 pi r))))
  
(perim s)
(perim r)
(perim t)