```
% Лабораторная работа №3. Типы данных. Модульное тестирование
% 14 января 2023 г.
% Павлов Иван, ИУ9-12Б
```



# Лабораторная работа №3

## Цели работы

- На практике ознакомиться с системой типов языка Scheme.

- На практике ознакомиться с юнит-тестированием.

- Разработать свои средства отладки программ на языке Scheme.

- На практике ознакомиться со средствами метапрограммирования языка
  Scheme.

## Реализация

1. Реализован макрос `trace` для трассировки:

   ```scheme
   (define-syntax trace-ex
     (syntax-rules ()
       ((trace-ex expr)
        (begin
          (write 'expr)
          (write " => ")
          (let ((res expr))
            (write res)
            (newline)
            res)))))
   ```

   Пример использования в коде:

   ```scheme
   (define (zip . xss)
     (if (or (null? xss)
             (null? (trace-ex (car xss))))
         '()
         (cons (map car xss)
               (apply zip (map cdr (trace-ex xss))))))
   ```

2. Реализован каркас для юнит-тестирования:

   ```scheme
   (define-syntax test
     (syntax-rules ()
       ((_ expr expected-result)
        (list (quote expr) expected-result))))
   
   (define ie (interaction-environment))
   
   (define (run-tests tests)
     (define (loop correct? tests)
       (if (null? tests)
           correct?
           (let ((expr (caar tests)))
             (write expr)
             (let* ((result (eval expr ie))
                    (check (equal? result (cadar tests))))
               (if check
                   (begin
                     (display " ok") 
                     (newline))
                   (begin
                     (display " FAIL")
                     (newline)
                     (display "  Expected: ") (write (cadar tests))
                     (newline)
                     (display "  Returned: ") (write result) 
                     (newline)))
               (loop (and correct? check) (cdr tests))))))
     (loop #t tests))
   
   (define (run-test test)
     (run-tests (list test)))
   ```

   Использование в коде:

   ```scheme
   (define (signum x)
     (cond
       ((< x 0) -1)
       ((= x 0)  1)
       (else     1)))
   
   (define counter 5)
   (define (next)
     (set! counter
           (+ counter 1))
     counter)
   
   (define the-tests
     (list (test (signum -2) -1)
           (test (signum  0)  0)
           (test (signum  2)  1)))
   
   (define next-tests
     (list (test (next) 6)
           (test (next) 8)
           (test (next) 8)))
   ```

3. Реализована процедура `ref` для доступа к произвольному элементу 
   последовательности:

   ```scheme
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
   ```

4. Реализована процедура `factorize` для разложения многочленов:

   ```scheme
   (define (factorize expr)
     (let ((a (cadr (cadr expr)))
           (b (cadr (caddr expr))))
       (if (and (= (caddr (cadr expr)) 2) 
                (= (caddr (caddr expr)) 2))
           (factorize-2 expr a b)
           (factorize-3 expr a b))))
   
   (define (factorize-2 expr a b)
     `(* (- ,a ,b) (+ ,a ,b)))
   
   (define (factorize-3 expr a b)
     (let ((sign (car expr)))
       `(* (,sign ,a ,b) (+ (- (expt ,a 2) (* ,a ,b)) 
                            (expt ,b 2)))))
   
   ; Тестирование
   
   (define factorize-tests
     (list (test (factorize '(- (expt x 2) (expt y 2)))
                 '(* (- x y) (+ x y)))
           (test (factorize '(- (expr x 3) (expr y 3)))
                 '(* (- x y) (+ (- (expt x 2) (* x y)) 
                                (expt y 2))))))
   ```



## Тестирование

1. Трассировка:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (zip '(1 2 3) '(one two three))
   (car xss)" => "(1 2 3)
   xss" => "((1 2 3) (one two three))
   (car xss)" => "(2 3)
   xss" => "((2 3) (two three))
   (car xss)" => "(3)
   xss" => "((3) (three))
   (car xss)" => "()
   ((1 one) (2 two) (3 three))
   ```

2. Юнит-тестирование:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (run-tests the-tests)
   (signum -2) ok
   (signum 0) FAIL
     Expected: 0
     Returned: 1
   (signum 2) ok
   #f
   > (run-tests next-tests)
   (next) ok
   (next) FAIL
     Expected: 8
     Returned: 7
   (next) ok
   #f
   ```

3. Доступ к произвольному элементу последовательности:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (ref '(1 2 3) 1)
   2
   > (ref #(1 2 3) 1)
   2
   > (ref "123" 1)
   #\2
   > (ref "123" 3)
   #f
   > (ref '(1 2 3) 1 0) 
   (1 0 2 3)
   > (ref #(1 2 3) 1 #\0) 
   #(1 #\0 2 3)
   > (ref "123" 1 0) 
   #f
   ```

4. Процедура `factorize`:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (run-tests factorize-tests)
   (factorize '(- (expt x 2) (expt y 2))) ok
   (factorize '(- (expr x 3) (expr y 3))) ok
   #t
   > (eval (list (list 'lambda 
                     '(x y) 
                     (factorize '(- (expt x 2) (expt y 2))))
               1 2)
         (interaction-environment))  
   -3
   ```



## Выводы

- [x] Произведено знакомство с тестированием и отладкой программ на Scheme
- [x] Разработаны собственные средства отладки программ на Scheme
- [x] Разработаны полезные макросы и каркасы для тестирования
