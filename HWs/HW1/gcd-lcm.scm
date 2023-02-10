(define (my-gcd a b)
  (define (loop x y)
    (cond ((or (= 0 x) (= 0 y)) 0)
          ((> x y) (loop (- x y) y))
          ((< x y) (loop x (- y x)))
          (else x)))
  (loop a b))

(define (my-lcm a b)
  (/ (* a b) (my-gcd a b)))

(define (prime? n)
  (define (loop i)
    (if (not (or (= 0 (remainder n i)) (> (* i i) n)))
        (loop (+ 1 i))
        (not (= 0 (remainder n i)))))
  (and (loop 4) (not (= 1 n)) (not (< n 0))))


