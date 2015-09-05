(define (is-atom? S)
  (not (pair? S)))


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