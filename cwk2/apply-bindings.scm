(define (is-atom? S)
  (not (pair? S)))


(define (fold-r func res lst)
  (if (null? lst)
      res
      (func (car lst)
            (fold-r func res (cdr lst)))))


(define (clause-match S L)
  (cond ((and (null? S) (null? L)) '())
        ((null? S) #f)
        ((and (null? L) (is-atom? S)) (list (cons S L)))
        ((and (is-atom? S) (pair? L)) (list (cons S L)))
        ((is-atom? S) #f)
        ((equal? (car S) (car L)) (clause-match (cdr S) (cdr L)))
        ((equal? (substring (symbol->string (car S)) 0 1) "?")
         (let ((ret (clause-match (cdr S) (cdr L))))
           (if (not ret)
               #f
               (cons
                (cons (car S) (car L))
                ret))))
        (else #f)))


(define (find-replace x y S)
  (if (is-atom? S)
      S
      (if (equal? (car S) x)
          (cons y (cdr S))
          (cons (car S)
                (find-replace x y (cdr S))))))


(define (apply-bindings binds pattern)
  (if (not binds)
      pattern
      (fold-r
       (lambda (pair r)
         (find-replace (car pair) (cdr pair) r))
       pattern
       binds)))