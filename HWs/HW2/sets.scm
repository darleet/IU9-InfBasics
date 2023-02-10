; Во всех процедурах сложность O(n^2) из-за
; операции member(), сложность которой O(n).

; Алгоритмическая сложность: O(n^2)
(define (list->set xs)
  (define (loop set xs)
    (cond ((null? xs) set)
          ((not (member (car xs) set)) (loop (cons (car xs) set) (cdr xs)))
          (else (loop set (cdr xs)))))
  (loop '() xs))

; Алгоритмическая сложность: O(n^2)
(define (set? xs)
  (define (loop xs el-rep?)
      (if (or el-rep? (null? xs))
          (not el-rep?)
          (loop (cdr xs) (member (car xs) (cdr xs)))))
  (loop xs #f))

; Алгоритмическая сложность: O(n^2) 
(define (union xs ys)
  (define (loop set xs ys)
    (cond ((and (null? xs) (null? ys)) ; если закончились множества для перебора
           set)
          ((member (car ys) set) ; если элемент из ys уже был
           (loop set xs (cdr ys)))
          ((null? xs) ; если закончились xs
           (loop (cons (car ys) set) xs (cdr ys)))
          ((member (car xs) set) ; если вдруг xs не мн-во, сделаем его мн-вом :)
           (loop set (cdr xs) ys))
          (else ; иначе просто пройдём по xs 
           (loop (cons (car xs) set) (cdr xs) ys))))
  (loop '() xs ys))

; Алгоритмическая сложность: O(n^2)
(define (intersection xs ys)
  (define (loop xs)
    (cond ((null? xs) '())
          ((member (car xs) ys) (cons (car xs) (loop (cdr xs))))
          (else (loop (cdr xs)))))
  (loop xs))

; Алгоритмическая сложность: O(n^2)
(define (difference xs ys)
  (define (loop xs)
    (cond ((null? xs) '())
          ((not (member (car xs) ys)) (cons (car xs) (loop (cdr xs))))
          (else (loop (cdr xs)))))
  (loop xs))

; Алгоритмическая сложность: O(n^2)
(define (symmetric-difference xs ys)
  (define (loop set xs-tail ys-tail)
    (cond ((and (null? xs-tail) (null? ys-tail))
           set)
          ((and (null? xs-tail) (not (member (car ys-tail) xs)))
           (loop (cons (car ys-tail) set) xs-tail (cdr ys-tail)))
          ((null? xs-tail)
           (loop set xs-tail (cdr ys-tail)))
          ((not (member (car xs-tail) ys))
           (loop (cons (car xs-tail) set) (cdr xs-tail) ys-tail))
          (else
           (loop set (cdr xs-tail) ys-tail))))
  (loop '() xs ys))

; Алгоритмическая сложность: O(n^2)
(define (set-eq? xs ys)
  (define (loop status xs)
    (if (or (null? xs) (not status))
        status
        (loop (member (car xs) ys) (cdr xs)))) 
  (and (loop #t xs) (equal? (length xs) (length ys))))