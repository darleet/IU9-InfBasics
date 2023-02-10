```
% Лабораторная работа № 4. Метапрограммирование. Отложенные вычисления
% 15 января 2023 г.
% Павлов Иван, ИУ9-12Б
```



# Лабораторная работа №4

## Цели работы

На примере языка Scheme ознакомиться со средствами метапрограммирования 
(«код как данные», макросы) и подходами к оптимизации вычислений (мемоизация 
результатов вычислений, отложенные вычисления).

В работе также предлагается разработать дополнительное средство отладки 
программ — каркас для отладки с помощью утверждений. На этом примере 
предлагается ознакомится с типичным применением программирования с 
использованием продолжений.

## Реализация

#### 1. Продолжения.

Реализация каркаса для отладки с помощью assertions:

```scheme
(define call/cc call-with-current-continuation)
(define exit #f)

(define (use-assertions)
  (call/cc (lambda (c) (set! exit c))))

(define-syntax assert
  (syntax-rules ()
    ((_ expr)
     (if (not expr)
         (begin
           (display "FAILED: ")
           (display (quote expr))
           (newline)
           (exit))))))

(use-assertions)

(define (1/x x)
  (assert (not (zero? x)))
  (write (/ 1 x))
  (newline))
```

#### 2. Код как данные. Порты ввода-вывода.

- *Сериализация данных.* Процедуры для записи и чтения данных из файла:

  ```scheme
  (define (save-data var path)
    (call-with-output-file path
      (lambda (port)
        (write var port)
        (newline port))))
  
  (define (load-data path)
    (call-with-input-file path
      (lambda (port)
        (let loop ((data (read port)))
          (if (not (eof-object? data))
              (begin (write data) (loop (read port)))
              (newline))))))
  ```

- *Подсчет строк в текстовом файле:*

  ```scheme
  (define (line-count path)
    (call-with-input-file path
      (lambda (port)
        (define (loop counter prev-newline?)
          (let ((symbol (read-char port)))
            (cond ((and (equal? symbol #\newline) 
                        (not prev-newline?))
                   (loop (+ 1 counter) (equal? symbol #\newline)))
                  ((eof-object? symbol) counter)
                  (else (loop counter 
                              (equal? symbol #\newline))))))
        (loop 0 #\newline))))
  ```

#### 3. Мемоизация результатов вычислений.

Функция вычисления n-го "числа трибоначчи":

```scheme
(define trib-memo
  (let ((memo-res '()))
    (lambda (n)
      (let* ((step-res (assoc n memo-res)))
        (if step-res
            (cadr step-res)
            (let ((new-res
                   (cond ((<= n 1) 0)
                         ((= n 2) 1)
                         (else (+ (trib-memo (- n 3))
                                  (trib-memo (- n 2))
                                  (trib-memo (- n 1)))))))
              (set! memo-res (cons (list n new-res) memo-res))
              (display memo-res)
              (newline)
              new-res))))))
```

#### 4. Отложенные вычисления.

Реализация макроса `my-if`:

```scheme
(define-syntax my-if
  (syntax-rules ()
    ((_ predicate then else)
     (let ((a (delay then))
           (b (delay else)))
       (or (and predicate (force a)) (force b))))))
```

#### 5. Локальные определения.

Макросы my-let и my-let*:

```scheme
(define-syntax my-let
  (syntax-rules ()
    ((_ ((var value) ...) body ...)
     ((lambda (var ...) body ...) value ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () body ...)
     (my-let () body ...))
    ((my-let* ((var value)) body ...)
     (my-let ((var value)) body ...))
    ((my-let* ((var value) . other) body ...)
     ((lambda (var) (my-let* other body ...)) value))))
```

#### 6. Управляющие конструкции.

##### A. Условия *when* и *unless*

```scheme
(define-syntax when
  (syntax-rules ()
    ((_ cond? body ...)
     (if cond? (begin body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ cond? body ...)
     (if (not cond?) (begin body ...)))))
```

##### Б. Циклы *for*

```scheme
(define-syntax for
  (syntax-rules (in as)
    ((_ i in xs body ...)
     (let loop ((vals xs))
       (if (not (null? vals))
           (let ((i (car vals)))
             body ...
             (loop (cdr vals))))))
    ((_ xs as i body ...)
     (for i in xs body ...))))
```

##### В. Цикл *while*

```scheme
(define-syntax while
  (syntax-rules ()
    ((while cond? body ...)
     (let loop ()
       (if cond? (begin body ... (loop)))))))
```

##### Г. Цикл *repeat*...*until*

```scheme
(define-syntax repeat
  (syntax-rules (until)
    ((repeat (body ...) until cond?)
     (let loop ()
       body ...
       (if (not cond?) (loop))))))
```

##### Д. Вывод в стиле С++

```scheme
(define-syntax cout
  (syntax-rules (<< endl)
    ((_ << endl)
     (newline))
    ((_ << endl . expr)
     (begin (newline)
            (cout . expr)))
    ((_ << expr1)
     (display expr1))
    ((_ << expr1 . expr)
     (begin (display expr1)
            (cout . expr)))))
```



## Тестирование

#### 1. Продолжения.

```scheme
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (map 1/x '(1 2 3 4 5))
1
1/2
1/3
1/4
1/5
> (map 1/x '(-2 -1 0 1 2))
-1/2
-1
FAILED: (not (zero? x))
```

#### 2. Код как данные. Порты ввода-вывода.

```scheme
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (save-data 56 "test.txt")
> (load-data "test.txt")
56
> (line-count "test.txt")
1
```

#### 3. Мемоизация результатов вычислений.

```scheme
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (trib-memo 5)
4
> (trib-memo 100)
53324762928098149064722658
```

#### 4. Отложенные вычисления.

```scheme
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (my-if #t 1 (/ 1 0))
1
> (my-if #f (/ 1 0) 1)
1
```

#### 5. Локальные определения.

```scheme
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (my-let ((a (+ 5 10))) (* a 2))
30
> (my-let* ((a (* 5 6)) (b (* a 3))) b)
90
```

#### 6. Управляющие конструкции.

##### А. Условия *when* и *unless*

```scheme
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (define x 1)
> (when   (> x 0) (display "x > 0")  (newline))
x > 0
> (unless (= x 0) (display "x != 0") (newline))
x != 0
```

##### Б. Циклы *for*

```scheme
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline)))
(1 4)
(1 5)
(1 6)
(2 4)
(2 5)
(2 6)
(3 4)
(3 5)
(3 6)
> (for '(1 2 3) as i
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))
(1 4)
(1 5)
(1 6)
(2 4)
(2 5)
(2 6)
(3 4)
(3 5)
(3 6)
```

##### В. Цикл *while*

```scheme
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))
(0 0)
(0 1)
(0 2)
(1 0)
(1 1)
(1 2)
(2 0)
(2 1)
(2 2)
```

##### Г. Цикл *repeat...until*

```scheme
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))
(0 0)(0 1)(0 2)
(1 0)(1 1)(1 2)
(2 0)(2 1)(2 2)
```

##### Д. Вывод в стиле C++

```scheme
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (cout << "a = " << 1 << endl << "b = " << 2 << endl)
a = 1
b = 2
```



## Выводы

- [x] Реализован собственный каркас для отладки программ
- [x] Реализована процедура с мемоизацией вычислений
- [x] Реализованы управляющие конструкции из императивных ЯП
- [x] Разработаны процедуры для работы с данными и файлами
