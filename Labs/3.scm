(load "trace.scm")
(load "unit-test.scm")

;---1---

(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss))))
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss))))))


;---2---

(define counter 5)
(define (next)
  (set! counter
        (+ counter 1))
  counter)

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1)
    (else     1)))

(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))

(define next-tests
  (list (test (next) 6)
        (test (next) 8)
        (test (next) 8)))

(run-tests the-tests)
(run-tests next-tests)


;---3---

(define (convert seq)
  (cond ((vector? seq) (vector->list seq))
        ((string? seq) (string->list seq))
        (else seq)))

(define (convert-reverse type seq)
  (cond ((equal? type 'vector) (list->vector seq))
        ((equal? type 'string) (list->string seq))
        (else seq)))

(define (type? seq)
  (cond ((vector? seq) 'vector)
        ((string? seq) 'string)
        (else 'list)))

(define (ref seq-input . ident)
  (let ((seq (convert seq-input))
        (index (car ident))       ; индекс для поиска/вставки
        (element (cdr ident)))    ; элемент для вставки
    (if (null? element)
        (ref-find seq index)
        (ref-insert seq index element (type? seq-input)))))

(define (ref-find seq index)
  (define (loop seq step)
    (cond ((= step index) (car seq))
          ((null? (cdr seq)) #f)
          (else (loop (cdr seq) (+ 1 step)))))
  (loop seq 0))

(define (ref-insert seq index element seq-type)
  (let ((status
         (and (equal? seq-type 'string)
              (not (char? (car element))))))
    (define (loop seq-head seq-tail step)
      (cond (status (not status))
            ((= step index)
             (convert-reverse
              seq-type
              (append seq-head element seq-tail)))
            ((not (null? seq-tail))
             (loop
              (append seq-head (list (car seq-tail)))
              (cdr seq-tail)
              (+ 1 step)))
            (else (not (null? seq-tail)))))
    (loop '() seq 0)))

   
(ref '(1 2 3) 1 0)  
(ref #(1 2 3) 1 0) 
(ref #(1 2 3) 1 #\0)
(ref "123" 1 #\0)   
(ref "123" 1 0)     
(ref "123" 3 #\4)    
(ref "123" 5 #\4)


;---4---

(define (factorize expr)
  (let ((a (cadr (cadr expr)))
        (b (cadr (caddr expr))))
    (if (and (= (caddr (cadr expr)) 2) (= (caddr (caddr expr)) 2))
        (factorize-2 expr a b)
        (factorize-3 expr a b))))

(define (factorize-2 expr a b)
  `(* (- ,a ,b) (+ ,a ,b)))

(define (factorize-3 expr a b)
  (let ((sign (car expr)))
    `(* (,sign ,a ,b) (+ (- (expt ,a 2) (* ,a ,b)) (expt ,b 2)))))

; Тестирование

(define factorize-tests
  (list (test (factorize '(- (expt x 2) (expt y 2)))
              '(* (- x y) (+ x y)))
        (test (factorize '(- (expr x 3) (expr y 3)))
              '(* (- x y) (+ (- (expt x 2) (* x y)) (expt y 2))))))

(run-tests factorize-tests)

(factorize '(- (expt x 2) (expt y 2)))
(factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))

(eval (list (list 'lambda 
                  '(x y) 
                  (factorize '(- (expt x 2) (expt y 2))))
            1 2)
      (interaction-environment))  

