(define (node-degree graph)
  (map (lambda (x)
         (cons (car x) (length (cdr x)))) graph))