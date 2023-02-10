```
% Домашнее задание № 7. Оболочка и скрипты
% 17 января 2023 г.
% Павлов Иван, ИУ9-12Б
```



# Домашнее задание №7

## Цель работы

Получение навыков написания сценариев на «скриптовых» языках.

## Реализация

#### 1. Утилита `tree`

Реализована на языке Perl:

```perl
#!/usr/bin/env perl
use Term::ANSIColor;
use autodie;
use List::MoreUtils 'first_index';

sub print_tree {
    my $path = shift;
    opendir(DIR, $path);
    my @files = sort(grep(!/\.\.$/m && !/\.$/m && (-d "$path/$_" || !$dir_only), readdir(DIR)));
    closedir(DIR);

    my $length = @files;
    my $counter = 0;
    my $depth = shift;
    my $edge = shift;

    foreach $file (@files) {
        $counter += 1;
        
        for($i=0;$i<$edge;$i++) { print "    "; }
        for($i=0;$i<$depth-$edge;$i++) { print "│   "; }

        if($counter == $length) {
            print "└── ";
        } else { 
            print "├── ";
        }

        if(-f "$path/$file") {
            if(-x "$path/$file") {
                print colored("$file\n", 'bold green');
            } else {
                print "$file\n";
            }
            $counter_files += 1; 
        } elsif(-d "$path/$file") {
            print colored("$file\n", 'bold blue');
            $counter_dirs += 1;
            if(($counter == $length) && (-r "$path/$file")) {
                print_tree("$path/$file", $depth+1, $edge+1);
            } elsif(-r "$path/$file") {
                print_tree("$path/$file", $depth+1, $edge);
            }
        }
    }
}

$dir_only = 0;
if(grep(/-d/, @ARGV)) {
    $dir_only = 1;
}

if(grep(/-o/, @ARGV)) {
    $output_path = $ARGV[(first_index {/-o/} @ARGV) + 1];
    if($output_path) {
        open($OUTPUT, '>', $output_path);
        select $OUTPUT;
    } else {
        print "Incorrect output path!";
        exit 1;
    }
}

print colored(".\n", 'bold blue');
$counter_dirs = 0;
$counter_files = 0;
print_tree(".", 0, 0);

if(!$dir_only) {
    print "\n$counter_dirs directories, $counter_files files\n";
} else {
    print "\n$counter_dirs directories\n";
}
```



## Тестирование

#### 1. Утилита `tree`

В консоли реализована цветная подсветка (однако она не видна в отчете):

```
darleet@DARLEET:~$ chmod +x my_tree
darleet@DARLEET:~$ ./my_tree
.
├── file1
├── file2
├── my_grep
├── my_tree
├── temp
│   └── file3
├── temp2
│   ├── docs
│   │   ├── file5
│   │   └── file6
│   └── file4
├── test.txt
└── tree.txt

3 directories, 10 files
darleet@DARLEET:~$ ./my_tree -d
.
├── temp
└── temp2
    └── docs

3 directories
```



## Выводы

- [x] Получены навыки работы с файловой системой при помощи Perl
- [x] Приобретены навыки по написанию собственных утилит на Perl