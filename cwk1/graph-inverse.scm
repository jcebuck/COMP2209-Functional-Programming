(define (get-parents list-pairs)
  (map (lambda (x)
         (car x)) list-pairs))


(define (graph-inverse g)
  (define (find-parents graph)
    (if (null? graph)
        '()
        (let ((node (caar graph)))
          (cons
           (cons node
                 (get-parents (filter (lambda (y) (memq node (cdr y))) g)))
           (find-parents (cdr graph))))))
  (find-parents g))