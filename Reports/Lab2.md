```
% Лабораторная работа № 2. Рекурсия, процедуры высшего порядка, обработка списков
% 17 декабря 2022 г.
% Павлов Иван, ИУ9-12Б
```



# Лабораторная работа №2

## Цель работы

Приобретение навыков работы с основами программирования на языке Scheme:
использование рекурсии, процедур высшего порядка, списков.

## Реализация

1. Реализована процедура `(count x xs)`
   
   ```scheme
   (define (count x xs)
     (define (loop xs res)
       (if (null? xs)
           res
           (loop (cdr xs)
                 (if (equal? x (car xs))
                     (+ res 1)
                     res))))
     (loop xs 0))
   ```

2. Реализована процедура `(delete pred? xs)`

   ```scheme
   (define (delete pred? xs)
     (define (loop xs res)
       (if (null? xs)
           (reverse res)
           (loop (cdr xs)
                 (if (not (pred? (car xs)))
                     (cons (car xs) res)
                     res))))
     (loop xs '()))
   ```

3. Реализована процедура `(iterate f x n)`

   ```scheme
   (define (iterate f x n)
     (define (loop x n res)
       (if (= n 0)
           (reverse res)
           (loop (f x)
                 (- n 1)
                 (cons x res))))
     (loop x n '()))
   ```

4. Реализована процедура `(intersperse e xs)`

   ```scheme
   (define (intersperse e xs)
     (define (loop xs res)
       (if (null? xs)
           (reverse res)
           (loop (cdr xs)
                 (if (equal? (list (car xs)) xs)
                     (cons (car xs) res)
                     (append (list e (car xs)) res)))))
     (loop xs '()))
   ```

5. Реализована процедура `(any? pred? xs)`

   ```scheme
   (define (any? pred? xs)
     (define (loop xs res)
       (if (null? xs)
           res
           (loop (cdr xs)
                 (or (pred? (car xs)) res))))
     (loop xs #f))
   ```

   А также процедура `(all? pred? xs)`

   ```scheme
   (define (all? pred? xs)
     (define (loop xs res)
       (if (null? xs)
           res
           (loop (cdr xs)
                 (and (pred? (car xs)) res))))
     (loop xs #t))
   ```

6. Реализована композиция функций в виде процедуры `(o . procs)`

   ```scheme
   (define (o . procs)
     (define (loop procs)
       (if (null? procs)
           (lambda (x) x)
           (let ((proc (car procs)))
             (if (null? (cdr procs))
                 proc
                 (lambda (x) (proc ((loop (cdr procs)) x)))))))
     (loop procs))
   ```

   

## Тестирование

1. Тесты для `(count x xs)`:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (count 'a '(a b c a))
   2
   > (count 'b '(a c d))
   0
   > (count 'a '())
   0
   ```

2. Тесты для `(delete pred? xs)`:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (delete even? '(0 1 2 3))
   (1 3)
   > (delete even? '(0 2 4 6))
   ()
   > (delete even? '(1 3 5 7))
   (1 3 5 7)
   > (delete even? '())
   ()
   ```

3. Тесты для `(iterate f x n)`:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (iterate (lambda (x) (* 2 x)) 1 6)
   (1 2 4 8 16 32)
   > (iterate (lambda (x) (* 2 x)) 1 1)
   (1)
   > (iterate (lambda (x) (* 2 x)) 1 0)
   ()
   ```

4. Тесты для `(intersperse e xs)`:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (intersperse 'x '(1 2 3 4))
   (1 x 2 x 3 x 4)
   > (intersperse 'x '(1 2))
   (1 x 2)
   > (intersperse 'x '(1))
   (1)
   > (intersperse 'x '())
   ()
   ```

5. Тесты для `(any? pred? xs)`:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (any? odd? '(1 3 5 7))
   #t
   > (any? odd? '(0 1 2 3))
   #t
   > (any? odd? '(0 2 4 6))
   #f
   > (any? odd? '())
   #f
   ```

   А также для `(all? pred? xs)`:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (all? odd? '(1 3 5 7))
   #t
   > (all? odd? '(0 1 2 3))
   #f
   > (all? odd? '(0 2 4 6))
   #f
   > (all? odd? '())
   #t
   ```

6. Тесты для `(o . procs)`:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (define (f x) (+ x 2))
   > (define (g x) (* x 3))
   > (define (h x) (- x))
   > ((o f g h) 1)
   -1
   > ((o f g) 1)
   5
   > ((o h) 1)
   -1
   > ((o) 1)
   1
   ```

   

## Вывод

- [x] Были приобретены навыки работы с процедурами высшего порядка, 
списками и рекурсиями
- [x] Реализованы основные процедуры для работы со списками: анализ,
поиск, вставка и удаление элементов
- [x] Реализована процедура композиции функций
