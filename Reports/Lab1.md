```
% Лабораторная работа № 1. Основы ЯП Scheme и среды разработки DrRacket
% 17 декабря 2022 г.
% Павлов Иван, ИУ9-12Б
```



# Лабораторная работа №1

## Цель работы

Приобретение навыков работы с основами программирования на языке Scheme
в среде DrRacket.

## Реализация

1. На лабораторной работе были показаны некоторые примеры конструкций
   на языке scheme, которые мной были воспроизведены в среде DrRacket.
   
   ```scheme
   (define (circ-len r) ; Длина окружности
     (* 2 pi r))
   
   (define (circ-area r) ; Площадь круга
     (* pi r r))
   
   (define (my-positive? x)
     (> x 0))
   
   (define (signum x) ; sign(x)
     (if (my-positive? x)
         +1
         (if (= 0 x)
             0
             -1)))
   
   (define (my-abs x) ; Абсолютное значение (модуль) числа
     (if (> x 0)
         x
         (- x)))
   
   (define (! n) ; Факториал n
     (if (> n 0)
         (* (! (- n 1)) n)
         1))
   
   (define (my-* x y)
     (if (> y 0)
         (+ (my-* x (- y 1)) x)
         0))
   ```




2. Реализованы предикаты `my-odd?` и `my-even?` для проверки десятичного
   числа на четность.

   ```scheme
   (define (my-odd? x)
     (= (remainder x 2) 1))
   
   (define (my-even? x)
     (= (remainder x 2) 0))
   ```



3. Реализована процедура `(power b e)`, которая вычисляет степень *b<sup>e</sup>.* 

   ```scheme
   (define (power b e)
     (if (= b 0)
         0
         (if (> e 0)
             (* (power b (- e 1)) b)
             (if (< e 0)
                 (/ (power b (+ e 1)) b)
                 1))))
   ```

   

## Тестирование

1. Тесты для некоторых воспроизведенных процедур:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (circ-len 10)
   62.83185307179586
   > (circ-area 5)
   78.53981633974483
   > (signum 2)
   1
   > (signum -4)
   -1
   > (signum 0)
   0
   > (my-abs -5)
   5
   > (! 5)
   120
   > (my-* 5 8)
   40
   ```



2. Тесты для `my-odd?` и `my-even?`:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (my-odd? 3)
   #t
   > (my-odd? 4)
   #f
   > (my-even? 0)
   #t
   > (my-even? 5)
   #f
   ```

   

3. Тесты для процедуры возведения в степень:

   ```scheme
   Welcome to DrRacket, version 8.6 [cs].
   Language: R5RS; memory limit: 128 MB.
   > (power 2 5)
   32
   > (power 5 -2)
   1/25
   ```



## Вывод

- [x] Были приобретены основные навыки программирования на языке Scheme,
изучен синтаксис языка, выполнены ознакомительные задания.
- [x] Изучены конструкция `define` и формы `if`, `cond`, `else`, а также основные
значения и логические операции языка Scheme.