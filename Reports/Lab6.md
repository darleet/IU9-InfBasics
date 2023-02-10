```
% Лабораторная работа № 6. Основы синтаксического и лексического 
анализа
% 15 января 2023 г.
% Павлов Иван, ИУ9-12Б
```



# Лабораторная работа №6

## Цель работы

Получение навыков реализации лексических анализаторов и нисходящих 
синтаксических анализаторов, использующих метод рекурсивного спуска.

## Реализация

1. Реализованы простейшие сканеры

   - Процедура `check-frac`:

     ```
     <Expression>      ::= <Signed-Num> "/" <Unsigned-Num>
     <Signed-Num>      ::= "+" <Unsigned-Num> | "-" <Unsigned-Num> 
                           | <Unsigned-Num>
     <Unsigned-Num>    ::= DIGIT <Number-Tail>
     <Number-Tail>     ::= DIGIT <Number-Tail> | <Empty>
     <Empty>           ::=
     ```

     ```scheme
     (define call/cc call-with-current-continuation)
     
     (define (check-frac str)
       (define stream (make-stream (string->list str) 'eos))
       (define (expression stream error)
         (begin
           (signed-num stream error)
           (expect #\/ stream error)
           (unsigned-num stream error)))
       (define (expect term stream error)
         (if (equal? (peek stream) term)
             (next stream)
             (error #f)))
       (define (signed-num stream error)
         (if (or (equal? #\+ (peek stream)) 
                 (equal? #\- (peek stream)))
             (begin (next stream) (unsigned-num stream error))
             (unsigned-num stream error)))
       (define (unsigned-num stream error)
         (if (and (char? (peek stream)) 
                  (char-numeric? (peek stream)))
             (begin (next stream) (num-tail stream error))
             (error #f)))
       (define (num-tail stream error)
         (if (and (char? (peek stream)) 
                  (char-numeric? (peek stream)))
             (begin (next stream) (num-tail stream error))
             #t))
       (call/cc
        (lambda (error)
          (expression stream error)
          (equal? (peek stream) 'eos))))
     ```

   - Процедура `scan-frac`:

     ```
     <Expression>      ::= <Signed-Num> "/" <Unsigned-Num>
     <Signed-Num>      ::= "+" <Unsigned-Num> | "-" <Unsigned-Num> 
                           | <Unsigned-Num>
     <Unsigned-Num>    ::= DIGIT <Number-Tail>
     <Number-Tail>     ::= DIGIT <Number-Tail> | <Empty>
     <Empty>           ::=
     ```

     ```scheme
     (define call/cc call-with-current-continuation)
     
     (define (scan-frac str)
       (define stream (make-stream (string->list str) 'eos))
       (define (expression stream error)
         (begin
           (define numerator (signed-num stream error))
           (expect #\/ stream error)
           (/ numerator (unsigned-num stream error))))
       (define (expect term stream error)
         (if (equal? (peek stream) term)
             (next stream)
             (error #f)))
       (define (signed-num stream error)
         (cond ((equal? #\+ (peek stream))
                (begin 
                 (next stream) 
                 (unsigned-num stream error)))
               ((equal? #\- (peek stream))
                (begin 
                 (next stream) 
                 (- (unsigned-num stream error))))
               (else (unsigned-num stream error))))
       (define (unsigned-num stream error)
         (if (and (char? (peek stream)) 
                  (char-numeric? (peek stream)))
             (string->number 
              (list->string (cons (next stream)
                                  (num-tail stream error))))
             (error #f)))
       (define (num-tail stream error)
         (if (and (char? (peek stream)) 
                  (char-numeric? (peek stream)))
             (cons (next stream) (num-tail stream error))
             '()))
       (call/cc
        (lambda (error)
          (define result (expression stream error))
          (and (equal? (peek stream) 'eos) result))))
     ```

   - Процедура `scan-many-fracs`:

     ```
     <Expr-List>    ::= <Spaces> <Expr> <Spaces> <Expr_List> 
                        | <Empty>
     <Spaces>       ::= WHITESPACE <Spaces> | <Empty>
     <Expr>         ::= <Signed-Num> "/" <Unsigned-Num>
     <Signed-Num>   ::= "+" <Unsigned-Num> | "-" <Unsigned-Num> 
                        | <Unsigned-Num>
     <Unsigned-Num> ::= DIGIT <Number-Tail>
     <Number-Tail>  ::= DIGIT <Number-Tail> | <Empty>
     <Empty>        ::=
     ```

     ```scheme
     (define (scan-many-fracs str)
       (define stream (make-stream (string->list str) 'eos))
       (define (expr-list stream error)
         (spaces stream error) ; Убрали пробелы перед дробью
         (if (and (char? (peek stream))
                  (or (char-whitespace? (peek stream))
                      (char-numeric? (peek stream))
                      (equal? (peek stream) #\+)
                      (equal? (peek stream) #\-)))
             (let ((expr-new (expr stream error)))
               (spaces stream error) ; Убрали пробелы после дроби
               (cons expr-new (expr-list stream error)))
             '()))
       (define (expr stream error)
         (define numerator (signed-num stream error))
         (expect #\/ stream error)
         (/ numerator (unsigned-num stream error)))
       (define (expect term stream error)
         (if (equal? (peek stream) term)
             (next stream)
             (error #f)))
       (define (spaces stream error)
         (if (and (char? (peek stream)) 
                  (char-whitespace? (peek stream)))
             (begin
               (next stream)
               (spaces stream error))
             #t))
       (define (signed-num stream error)
         (cond ((equal? #\+ (peek stream))
                (begin
                  (next stream)
                  (unsigned-num stream error)))
               ((equal? #\- (peek stream))
                (begin
                  (next stream)
                  (- (unsigned-num stream error))))
               (else (unsigned-num stream error))))
       (define (unsigned-num stream error)
         (if (and (char? (peek stream)) 
                  (char-numeric? (peek stream)))
             (string->number 
              (list->string (cons (next stream)
                                  (num-tail stream error))))
             (error #f)))
       (define (num-tail stream error)
         (if (and (char? (peek stream)) 
                  (char-numeric? (peek stream)))
             (cons (next stream) (num-tail stream error))
             '()))
       (call/cc
        (lambda (error)
          (define result (expr-list stream error))
          (and (equal? (peek stream) 'eos) result))))
     ```

2. Процедура `parse` для парсинга программы на модельном языке:

   ```scheme
   (define (parse tokens)
     (define stream (make-stream (vector->list tokens) "eof"))
     (define reserved-words '(define end endif))
     (define (expect term stream error)
       (if (equal? (peek stream) term)
           (next stream)
           (error #f)))
     (define (program stream error)
       (list (articles stream error) (body stream error)))
     (define (articles stream error)
       (if (equal? (peek stream) 'define)
           (cons (article stream error) (articles stream error))
           '()))
     (define (article stream error)
       (let* ((_define (expect 'define stream error))
              (_word (next stream))
              (_body (body stream error))
              (_end (expect 'end stream error)))
         (if (not (member _word reserved-words))
             (list _word _body)
             (error #f))))
     (define (body stream error)
       (cond ((equal? (peek stream) 'if)
              (let* ((_if (next stream))
                     (_body (body stream error))
                     (_endif (expect 'endif stream error))
                     (_body-tail (body stream error)))
                (cons (list 'if _body) _body-tail)))
             ((integer? (peek stream))
              (let* ((_integer (next stream))
                     (_body-tail (body stream error)))
                (cons _integer _body-tail)))
             ((and (symbol? (peek stream))
                   (not (equal? (peek stream) 'end))
                   (not (equal? (peek stream) 'endif)))
              (let* ((_word (next stream))
                     (_body-tail (body stream error)))
                (cons _word _body-tail)))
             (else '())))
     (call/cc
      (lambda (error)
        (define result (program stream error))
        (and (equal? (peek stream) "eof") result))))
   ```

   

## Тестирование

1. Тесты для сканеров:

   ```scheme
   (define check-frac-tests
     (list (test (check-frac "110/111") #t)
           (test (check-frac "-4/3") #t)
           (test (check-frac "+5/10") #t)
           (test (check-frac "5.0/10") #f)
           (test (check-frac "FF/10") #f)))
   (run-tests check-frac-tests)
   
   (define scan-frac-tests
     (list (test (scan-frac "110/111") 110/111)
           (test (scan-frac "-4/3") -4/3)
           (test (scan-frac "+5/10") 1/2)
           (test (scan-frac "5.0/10") #f)
           (test (scan-frac "FF/10") #f)))
   (run-tests scan-frac-tests)
   
   (define scan-many-fracs-tests
     (list (test (scan-many-fracs "\t1/2 1/3\n\n10/8\t") 
                 '(1/2 1/3 5/4))
           (test (scan-many-fracs "\t1/2 1/3\n\n2/-5") 
                 #f)
           (test (scan-many-fracs "\t1/2 1/32/-5") 
                 #f)))
           (test (scan-many-fracs "") 
                 '())
           (test (scan-many-fracs " ") 
                 '())
   (run-tests scan-many-fracs-tests)
   ```

   Вывод:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   (check-frac "110/111") ok
   (check-frac "-4/3") ok
   (check-frac "+5/10") ok
   (check-frac "5.0/10") ok
   (check-frac "FF/10") ok
   #t
   (scan-frac "110/111") ok
   (scan-frac "-4/3") ok
   (scan-frac "+5/10") ok
   (scan-frac "5.0/10") ok
   (scan-frac "FF/10") ok
   #t
   (scan-many-fracs "\t1/2 1/3\n\n10/8\t") ok
   (scan-many-fracs "\t1/2 1/3\n\n2/-5") ok
   (scan-many-fracs "\t1/2 1/32/-5") ok
   (scan-many-fracs "") ok
   (scan-many-fracs " ") ok
   #t
   ```

2. Тесты для парсера:

   ```scheme
   (define parse-tests
     (list (test (parse #(1 2 +)) '(() (1 2 +)))
           (test (parse #(x dup 0 swap if drop -1 endif))
                 '(() (x dup 0 swap (if (drop -1)))))
           (test (parse #(define word w1 w2 w3))
                 #f)
           (test (parse #(0 if 1 if 2 endif 3 endif 4))
                 '(() (0 (if (1 (if (2)) 3)) 4)))
           (test (parse #(endif)) #f)
           (test (parse #(define define end)) #f)
           (test (parse #(define endif end)) #f)))
   (run-tests parse-tests)
   ```

   Вывод:

   ```scheme
   (parse #(1 2 +)) ok
   (parse #(x dup 0 swap if drop -1 endif)) ok
   (parse #(define word w1 w2 w3)) ok
   (parse #(0 if 1 if 2 endif 3 endif 4)) ok
   (parse #(endif)) ok
   (parse #(define define end)) ok
   (parse #(define endif end)) ok
   #t
   ```



## Выводы

- [x] Приобретены навыки разработки сканеров
- [x] Реализован собственный парсер программы на модульном языке