# Домашнее задание №1

При выполнении заданий **не используйте** присваивание и циклы. Избегайте возврата логических значений из условных конструкций. Подготовьте примеры для демонстрации работы разработанных вами процедур.

## 1. Определение дня недели по дате

Определите процедуру `day-of-week`, вычисляющую день недели по дате по григорианскому календарю. Воспользуйтесь алгоритмом, описанным в литературе. Пусть процедура принимает три формальных аргумента (день месяца, месяц и год в виде целых чисел) и возвращает целое число — номер дня в неделе (0 — воскресенье, 1 — понедельник, ... 6 — суббота).

Пример вызова процедуры:

```
(day-of-week 04 12 1975) ⇒ 4
(day-of-week 04 12 2006) ⇒ 1
(day-of-week 29 05 2013) ⇒ 3
```

## 2. Действительные корни квадратного уравнения

Определите процедуру, принимающую коэффициенты *a*, *b* и *c* квадратного уравнения вида *ax*²+*bx*+*c*=0 и возвращающую список чисел &mdash; корней уравнения (один или два корня, или пустой список, если корней нет).

**Указание:** для формирования списка используйте функцию `(list …)`:

```
(list)        → ()
(list 10)     → (10)
(list 10 11)  → (10 11)
```

## 3. НОД, НОК и проверка числа на простоту

Определите:

*  Процедуру `(my-gcd a b)`, возвращающую наибольший общий делитель чисел `a` и `b`. Поведение вашей процедуры должно быть идентично поведению встроенной процедуры `gcd`.

*  Процедуру `(my-lcm a b)`, возвращающую наименьшее общее кратное чисел `a` и `b`. Используйте процедуру `my-gcd`, определенную вами ранее. Поведение вашей процедуры должно быть идентично поведению встроенной процедуры `lcm`.

*  Процедуру `(prime? n)`, выполняющую проверку числа `n` на простоту и возвращающую `#t`, если число простое и `#f` в противном случае.

*  Примеры вызова процедур:

```
(my-gcd 3542 2464) ⇒ 154
(my-lcm 3 4)       ⇒  12
(prime? 11)        ⇒  #t
(prime? 12)        ⇒  #f
```