(define (day-of-week day month year)
  (let* ((a (quotient (- 14 month) 12))
         (y (- year a))
         (m (- (+ (* 12 a) month) 2))
         (// (lambda (a b) (quotient a b))))
    (remainder
     (+
      day
      y
      (// y 4)
      (- (// y 100))
      (// y 400)
      (// (* 31 m) 12)
      7000)
     7)))