(define (size-count linear-size sizes)
  (if (not (null? sizes))
      (size-count (* linear-size (car sizes)) (cdr sizes))
      linear-size))

(define (make-multi-vector sizes . fill)
  (if (null? fill)
      (list 'multi-vector sizes (make-vector (size-count 1 sizes)))
      (list 'multi-vector sizes (make-vector (size-count 1 sizes) (car fill)))))

(define (multi-vector? m)
  (and (not (vector? m)) (list? m) (equal? (car m) 'multi-vector)))

; Реализуем многомерность при помощи линейного расположения элементов

(define (find-index m indices)
  (let ((indices-rev (reverse indices))
        (sizes-rev (reverse (cadr m))))
    (define (loop pointer dim-linear-size indices-rev sizes-rev)
      (if (not (null? indices-rev))
          (loop (+ pointer (* (car indices-rev) (car sizes-rev))) ; Указатель
                (* dim-linear-size (car sizes-rev)) ; Линейный размер на итерации
                (cdr indices-rev)
                (cdr sizes-rev))
          pointer))
    (loop (car indices-rev) 1 (cdr indices-rev) sizes-rev)))
; сразу указали на индекс в последнем размере вектора

(define (multi-vector-set! m indices element)
  (vector-set! (caddr m) (find-index m indices) element))

(define (multi-vector-ref m indices)
  (vector-ref (caddr m) (find-index m indices)))