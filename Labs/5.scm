(define (interpret program stack)
  (define (stack-not n)
    (if (and n (not (equal? 0 n))) 0 -1))
  (define (stack-and n1 n2)
    (if (and (or (not (equal? 0 n1)) (not (equal? 0 n2))) n1 n2) -1 0))
  (define (stack-or n1 n2)
    (if (or n1 n2) -1 0))
  (define (find-end start)
    (if (equal? (vector-ref program start) 'end)
        (+ 1 start)
        (find-end (+ 1 start))))
  (define (find-endif start)
    (if (equal? (vector-ref program start) 'endif)
        (+ 1 start)
        (find-endif (+ 1 start))))
  (define (main index stack dict stack_return)
    (if (>= index (vector-length program))
        stack
        (let ((lexem (vector-ref program index)))
          (cond ((number? lexem)
                 (main (+ 1 index) (cons lexem stack) dict stack_return))
                ((equal? lexem '+)
                 (main (+ 1 index) (cons (+ (car stack) (cadr stack)) (cddr stack)) dict stack_return))
                ((equal? lexem '-)
                 (main (+ 1 index) (cons (- (cadr stack) (car stack)) (cddr stack)) dict stack_return))
                ((equal? lexem '*)
                 (main (+ 1 index) (cons (* (car stack) (cadr stack)) (cddr stack)) dict stack_return))
                ((equal? lexem '/)
                 (main (+ 1 index) (cons (/ (cadr stack) (car stack)) (cddr stack)) dict stack_return))
                ((equal? lexem 'mod)
                 (main (+ 1 index) (cons (remainder (cadr stack) (car stack)) (cddr stack)) dict stack_return))
                ((equal? lexem 'neg)
                 (main (+ 1 index) (cons (- (car stack)) (cdr stack)) dict stack_return))
                ((equal? lexem '=)
                 (main (+ 1 index) (cons (if (= (cadr stack) (car stack)) -1 0) (cddr stack)) dict stack_return))
                ((equal? lexem '>)
                 (main (+ 1 index) (cons (if (> (cadr stack) (car stack)) -1 0) (cddr stack)) dict stack_return))
                ((equal? lexem '<)
                 (main (+ 1 index) (cons (if (< (cadr stack) (car stack)) -1 0) (cddr stack)) dict stack_return))
                ((equal? lexem 'not)
                 (main (+ 1 index) (cons (stack-not (car stack)) (cdr stack)) dict stack_return))
                ((equal? lexem 'and)
                 (main (+ 1 index) (cons (stack-and (car stack) (cadr stack)) (cddr stack)) dict stack_return))
                ((equal? lexem 'or)
                 (main (+ 1 index) (cons (stack-or (car stack) (cadr stack)) (cddr stack)) dict stack_return))
                ((equal? lexem 'drop)
                 (main (+ 1 index) (cdr stack) dict stack_return))
                ((equal? lexem 'swap)
                 (main (+ 1 index) (cons (cadr stack) (cons (car stack) (cddr stack))) dict stack_return))
                ((equal? lexem 'dup)
                 (main (+ 1 index) (cons (car stack) (cons (car stack) (cdr stack))) dict stack_return))
                ((equal? lexem 'over)
                 (main (+ 1 index) (cons (cadr stack) stack) dict stack_return))
                ((equal? lexem 'rot)
                 (main (+ 1 index) (cons (caddr stack)
                                         (cons (cadr stack)
                                               (cons (car stack)
                                                     (cdddr stack)))) dict stack_return))
                ((equal? lexem 'depth)
                 (main (+ 1 index) (cons (length stack) stack) dict stack_return))
                ((equal? lexem 'define)
                 (main (find-end index) stack (cons (list (vector-ref program (+ 1 index)) (+ 2 index)) dict) stack_return))
                ((equal? lexem 'if)
                 (if (not (equal? (car stack) 0))
                     (main (+ 1 index) (cdr stack) dict stack_return)
                     (main (find-endif index) (cdr stack) dict stack_return)))
                ((equal? lexem 'endif) (main (+ 1 index) stack dict stack_return))
                ((equal? lexem 'end) (main (car stack_return) stack dict (cdr stack_return)))
                ((equal? lexem 'exit) (main (car stack_return) stack dict (cdr stack_return)))
                ((not (equal? (assq lexem dict) #f))
                 (main (cadr (assq lexem dict)) stack dict (cons (+ 1 index) stack_return)))
                (else 'error)))))
  (main 0 stack '() '()))

(interpret #(   define abs
                    dup 0 <
                    if neg endif
                end
                9 abs
                -9 abs      ) (quote ()))

(interpret #(   define =0? dup 0 = end
                define <0? dup 0 < end
                define signum
                    =0? if exit endif
                    <0? if drop -1 exit endif
                    drop
                    1
                end
                 0 signum
                -5 signum
                10 signum       ) (quote ()))

(interpret #(   define -- 1 - end
                define =0? dup 0 = end
                define =1? dup 1 = end
                define factorial
                    =0? if drop 1 exit endif
                    =1? if drop 1 exit endif
                    dup --
                    factorial
                    *
                end
                0 factorial
                1 factorial
                2 factorial
                3 factorial
                4 factorial     ) (quote ()))

(interpret #(   define =0? dup 0 = end
                define =1? dup 1 = end
                define -- 1 - end
                define fib
                    =0? if drop 0 exit endif
                    =1? if drop 1 exit endif
                    -- dup
                    -- fib
                    swap fib
                    +
                end
                define make-fib
                    dup 0 < if drop exit endif
                    dup fib
                    swap --
                    make-fib
                end
                10 make-fib     ) (quote ()))

(interpret #(   define =0? dup 0 = end
                define gcd
                  =0? if drop exit endif
                  swap over mod
                  gcd
                end
                90 99 gcd
                234 8100 gcd             ) '())
                