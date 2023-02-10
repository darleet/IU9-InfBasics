```
% Лабораторная работа № 7. Оболочка и скрипты
% 16 января 2023 г.
% Павлов Иван, ИУ9-12Б
```



# Лабораторная работа №7

## Цель работы

Получение навыков написания сценариев на «скриптовых» языках.

## Реализация

1. На Bash написан скрипт `exec_repeater`, который запускает некоторую 
   программу строго каждые *t* минут:

   ```bash
   #!/bin/bash
   
   show_help() {
       echo "Usage: ./exec_repeater [-h] <program> <interval>"
       echo "Repeats the <program> every <interval> minutes."
   }
   
   
   [ "$1" == "-h" ] && show_help && exit 0
   [ $# -ne 2 ] && show_help && exit 1
   
   program="$1"
   interval=$2
   
   STDOUT_FILE=./$program\_stdout
   STDERR_FILE=./$program\_stderr
   
   $program > $STDOUT_FILE 2> $STDERR_FILE &
   
   while true; do
       sleep $(( $interval * 60 ))
       if ! ps -p $! > /dev/null; then
           $program >> $STDOUT_FILE 2>> $STDERR_FILE &
       fi
   done
   ```

   Также, реализована долго выполняющаяся программа `awaiter`:

   ```bash
   #!/bin/bash
   time=120
   echo awaiting $time secs...
   sleep $time
   ```

2. Скрипт на Bash, который принимает путь к проекту на C и выводит общее 
   число непустых строк во всех файлах `.c` и `.h`:

   ```bash
   #!/bin/bash
   
   show_help() {
       echo "Usage: ./clines [-h] <path>"
       echo "Counts non-empty lines of C sourse files at <path>"
   }
   
   [ "$1" == "-h" ] && show_help && exit 0
   [ $# -ne 1 ] && show_help && exit 1
   
   files=$(find "$1" -name '*.c' -o -name '*.h')
   [ ! "$files" ] && echo "C source files were not found" && exit 1
   
   counter=0
   for file in $files; do
       (( counter += $(grep -c -v '^$' $file) ))
   done
   echo "Lines (nonempty) in total:" $counter
   ```

3. Программа, которая выводит в консоль указанное число строк заданной 
   длины, состоящих из латинских букв, цифр и печатных знаков (генератор 
   паролей) на Perl:

   ```perl
   #!/usr/bin/env perl
   use strict;
   
   use lib "./";
   use PasswordGen qw(gen_pass);
   
   sub MAIN {
       for(my $i=0;$i<$ARGV[1];$i++) {
           print gen_pass($ARGV[0]), "\n";
       }
   }
   
   MAIN;
   ```

   Модуль генерации пароля, используемый в программе:

   ```perl
   package PasswordGen;
   
   use Exporter qw(import);
   @EXPORT_OK = qw(gen_pass);
   
   sub gen_pass {
       my @symbols = ('a'...'z', 'A'...'Z', 0...9);
       my $length = shift;
       my $randpass = join "", map $symbols[rand @symbols], 1..$length;
       return $randpass;
   }
   
   1;
   ```

4. Функция на Perl с мемоизацией результатов вычислений:

   ```perl
   #!/usr/bin/env perl
   use strict;
   
   sub memo_func {
       my $func = shift;
       my %memory;
       return sub {
           my $key = join(",", @_);
           if($memory{$key}) {
               return $memory{$key};
           } else {
               print "calculated: ";
               $memory{$key} = $func->(@_);
               return $func->(@_);
           }
       }
   }
   
   # Функция для теста мемоизации
   sub test_func {
       my $a = shift;
       my $b = shift;
       return $a * $b;
   }
   
   my $test_memo = memo_func(\&test_func);
   print $test_memo->(1, 2), "\n";
   print $test_memo->(1, 2), "\n";
   print $test_memo->(3, 7), "\n";
   ```



## Тестирование

1. Скрипт `exec_repeater` будет запускать переданную в командной строке в 
   качестве аргумента программу `awaiter`:

   ```
   darleet@DARLEET:~$ chmod +x exec_repeater
   darleet@DARLEET:~$ ./exec_repeater awaiter 5
   ```

   Стандартный поток вывода будет перенаправлен в файл `awaiter_stdout`, а 
   поток ошибок в файл `awaiter_stderr`.

2. Скрипт поиска строк в файлах `.c` и `.h` был протестирован на файлах 
   решений задач по предмету Алгоритмы и Структуры Данных:

   ```
   darleet@DARLEET:~$ chmod +x clines
   darleet@DARLEET:~$ ./clines ~/BMSTU/ADS
   Lines (nonempty) in total: 1052
   ```

3. Результат работы генератора паролей на Perl:

   ```
   darleet@DARLEET:~$ chmod +x main_app
   darleet@DARLEET:~$ ./main_app 20 5
   wVJgFi24PlTAQ9Sloo9n
   urLV4Ab8rldtagZbyCJU
   HDQ8Mzvp0cUJFVEpJsT4
   qxaDqDGQ0S016cZvwcKs
   p5xZbx5NKig2Rtf1Em7U
   ```

4. Результат работы функции с мемоизацией на Perl:

   ```
   darleet@DARLEET:~$ chmod +x memo
   darleet@DARLEET:~$ ./memo
   calculated: 2
   2
   calculated: 21
   ```

   В результате видим, что значение для двух одинаковых аргументов не было 
   вычислено заново, а было взято из хэша.

## Выводы

- [x] Мной был изучен Perl, на котором были выполнены задания по работе с 
  функциями и основными типами данных языка
- [x] Получены навыки написания на Bash скриптов для работы с файлами