(define less-than
  (lambda (x y)
    (< (car x) (car y))))


(define insert
  (lambda (less-than x lst)
    (if (null? lst)
        (list x)
        (let ((y (car lst))
              (ys (cdr lst)))
          (if (less-than x y)
              (cons x lst)
              (cons y (insert less-than x ys)))))))


(define list-sort
  (lambda (less-than lst)
    (if (null? lst)
        '()
        (insert less-than
                (car lst)
                (list-sort less-than (cdr lst))))))


(define (get-parents list-pairs)
  (map (lambda (x)
         (car x)) list-pairs))


(define (node-degree graph)
  (map (lambda (x)
         (cons (car x) (length (cdr x)))) graph))


(define (group-by-degree node-degrees)
  (if (null? node-degrees)
      '()
      (let ((degree (cdar node-degrees)))
        (cons
         (cons degree
               (get-parents (filter (lambda (y) (eq? (cdr y) degree)) node-degrees)))
         (group-by-degree (filter (lambda (z) (not (eq? (cdr z) degree))) node-degrees))))))


(define (make-csv graph)
  (let ((degrees (list-sort less-than (group-by-degree (node-degree graph)))))
    (define (count d)
      (if (not (null? d))
          (string-append (number->string (caar d)) ", " (number->string(length (cdar d))) "\n" (count (cdr d)))
          ""
          ))
    (display (string-append "#Degree, Frequency\n" (count degrees)))))