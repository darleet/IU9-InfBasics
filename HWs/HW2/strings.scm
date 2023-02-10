(define (whitespace? char)
  (or
   (equal? char #\space)
   (equal? char #\newline)
   (equal? char #\return)
   (equal? char #\tab)))
      
; Алгоритмическая сложность - O(n), n - длина строки
(define (string-trim-left str)
  (let ((xs (string->list str)))
    (define (loop xs)
      (cond ((null? xs) '()) ; если вдруг нам передали пустой список
            ((whitespace? (car xs)) (loop (cdr xs)))
            (else xs)))
    (list->string (loop xs))))

; Алгоритмическая сложность - O(n^2) из-за процедуры append
(define (string-trim-right str)
  (let ((xs (string->list str)))
    (define (loop xs ws-buffer) ; создали буфер для пробельных символов
      (cond ((null? xs) '())
            ((whitespace? (car xs))
             (loop (cdr xs) (append ws-buffer (list (car xs)))))
            ((not (null? ws-buffer))
             (append ws-buffer (cons (car xs) (loop (cdr xs) '()))))
            (else
             (cons (car xs) (loop (cdr xs) '())))))
    (list->string (loop xs '()))))

; Алгоритмическая сложность - O(n^2)
(define (string-trim str)
  (string-trim-right (string-trim-left str)))

; для ачивки по list-trim-right аналогично:

(define (list-trim-right xs)
    (define (loop xs ws-buffer) ; создали буфер для пробельных символов
      (cond ((null? xs) '())
            ((whitespace? (car xs))
             (loop (cdr xs) (append ws-buffer (list (car xs)))))
            ((not (null? ws-buffer))
             (append ws-buffer (cons (car xs) (loop (cdr xs) '()))))
            (else
             (cons (car xs) (loop (cdr xs) '())))))
    (loop xs '()))

; Алгоритмическая сложность - O(n), n - длина a+b
(define (string-prefix? a b)
  (let ((la (string->list a))
        (lb (string->list b)))
    (define (loop la lb status)
      (cond ((or (null? la) (not status)) status) ; конец первой строки
            ((null? lb) (not status)) ; конец второй строки
            (else (loop (cdr la) (cdr lb) (equal? (car la) (car lb))))))
    (loop la lb #t)))

; Алгоритмическая сложность - O(n), n - длина a
(define (string-suffix? a b)
  (let ((la (string->list a))
        (lb (string->list b)))
    (define (loop buff-la lb)
      (cond ((null? lb) ; закончились обе строки? (если да => первая в конце второй)
             (null? buff-la))
            ((or (null? buff-la) (not (equal? (car buff-la) (car lb))))
             (loop la (cdr lb))) ; если элементы не равны или буфер закончился
            (else
             (loop (cdr buff-la) (cdr lb)))))
    (loop la lb)))

; Алгоритмическая сложность - O(n), n - длина b
(define (string-infix? a b)
  (let ((la (string->list a))
        (lb (string->list b)))
    (define (loop buff-la lb)
      (cond ((or (null? buff-la) (null? lb))
             (null? buff-la))
            ((equal? (car buff-la) (car lb))
             (loop (cdr buff-la) (cdr lb)))
            (else
             (loop la (cdr lb)))))
    (loop la lb)))

(define (string-list char)
  (list (list->string (list char))))

; Алгоритмическая сложность - O(n^2) из-за процедуры append
(define (string-split str sep)
  (let ((lstr (string->list str))
        (lsep-init (string->list sep)))
    (define (loop buff-lsep lstr lsep res)
      (cond ((null? lstr)
             (append res buff-lsep))
            ((null? lsep)
             (loop '()
                   (cdr lstr)
                   lsep-init
                   (append res (string-list (car lstr)))))
            ((equal? (car lstr) (car lsep))
             (loop (append buff-lsep (string-list (car lsep)))
                   (cdr lstr)
                   (cdr lsep)
                   res))
            (else
             (loop '()
                   (cdr lstr)
                   lsep-init
                   (append res buff-lsep (string-list (car lstr)))))))
    (loop '() lstr lsep-init '())))