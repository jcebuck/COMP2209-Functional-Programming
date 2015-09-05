(define (get-parents list-pairs)
  (map (lambda (x)
         (car x)) list-pairs))


(define (group-by-degree node-degrees)
  (if (null? node-degrees)
      '()
      (let ((degree (cdar node-degrees)))
        (cons
         (cons degree
               (get-parents (filter (lambda (y) (eq? (cdr y) degree)) node-degrees)))
         (group-by-degree (filter (lambda (z) (not (eq? (cdr z) degree))) node-degrees))))))