; Алгоритмическая сложность: O(n), n - длина списка
(define (my-range a b d)
  (define (loop element)
    (if (< element b)
        (cons element (loop (+ element d)))
        '()))
  (loop a))

; Алгоритмическая сложность: O(n), ачивки выполнены
(define (my-flatten xs)
  (define (loop elem-end xs-end)
    (if (not (null? xs-end)) ; если остались элементы в главном списке
        (cond ((and (not (null? elem-end)) (pair? (car xs-end))) 
               (loop elem-end (car xs-end))) ; войдем внутрь вл. списка во вл.списке
              ((and (not (null? elem-end)) (not (null? (cdr xs-end)))) 
               (cons (car xs-end) (loop elem-end (cdr xs-end)))) 
              ((pair? (car xs-end)) ; если сл. элемент - список (пара)
               (loop (cdr xs-end) (car xs-end)))
              ((not (null? elem-end)) ; пара вл. эл. и оставшихся в главном списке
               (cons (car xs-end) (loop '() elem-end)))
              (else
               (cons (car xs-end) (loop '() (cdr xs-end))))) ; обычный случай
        '()))
  (loop '() xs))

; Алгоритмическая сложность: O(n), n - кол-во элементов в списке
(define (my-element? x xs)
  (define (loop status xs)
    (if (not (null? xs))
        (loop (or (equal? (car xs) x) status) (cdr xs))
        status))
  (loop #f xs))

; Алгоритмическая сложность: O(n), n - кол-во элементов в списке
(define (my-filter pred? xs)
  (define (loop xs)
    (cond ((null? xs) '())
          ((pred? (car xs)) (cons (car xs) (loop (cdr xs))))
          (else (loop (cdr xs)))))
  (loop xs))

; Алгоритмическая сложность: O(n)
(define (my-fold-left op xs)
  (define (loop result xs)
    (if (null? xs)
        result
        (loop (op result (car xs)) (cdr xs))))
  (loop (car xs) (cdr xs)))

; Алгоритмическая сложность: O(n)
(define (my-fold-right op xs)
  (define (loop xs)
    (if (null? (cdr xs))
        (car xs)
        (op (car xs) (loop (cdr xs)))))
  (loop xs))
