(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) `(lazy-cons ,(cons a (delay b))))))

(define (lazy-car p)
  (caadr p))

(define (lazy-cdr p)
  (force (cdadr p)))

(define (lazy-head xs k)
  (define (loop xs i)
    (if (= i k)
        '()
        (cons (lazy-car xs) (loop (lazy-cdr xs) (+ i 1)))))
  (loop xs 0))

(define (lazy-ref xs k)
  (define (loop xs i)
    (if (= i k)
        (lazy-car xs)
        (loop (lazy-cdr xs) (+ i 1))))
  (loop xs 0))
  
(define (naturals n)
  (lazy-cons n (naturals (+ n 1))))

(define (factorials n)
  (lazy-cons (fact-find n) (factorials (+ n 1))))

(define (fact-find n)
  (define (loop n res)
    (if (zero? n)
        res
        (loop (- n 1) (* res n))))
  (loop n 1))

(define (lazy-factorial n)
  (lazy-ref (factorials 0) n))
