(define graph1
  '((entity e0 (prov:label "entity e0") (prov:location "London"))
    (entity e1)
    (entity e2)
    (entity e3)
    (entity e4 (prov:location "Southampton"))
    (entity e5)
    (wasDerivedFrom d0 e1 e0 (prov:type prov:Revision))
    (wasDerivedFrom d1 e2 e1 (prov:type prov:Revision))
    (wasDerivedFrom d2 e3 e2 (prov:type prov:Revision))
    (wasDerivedFrom d3 e4 e3 (prov:type prov:Revision))
    (wasDerivedFrom d4 e4 e5)
    (wasDerivedFrom d5 e5 e0)))


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


(define (pattern-match-graph patterns graph)
  (define (possible-bindings pattern)
    (filter (lambda (x) (not (eq? x #f)))
            (map (lambda (x) (clause-match pattern x))
                 graph)))
  
  (define (find-matches bindings patterns)
    (if (null? patterns)
        (list bindings)
        (fold-r (lambda (new-bindings r)
                 (append 
                  (find-matches (append bindings new-bindings) (cdr patterns))
                  r))
               '()
               (possible-bindings (apply-bindings bindings (car patterns))))))
  
  (find-matches '() patterns))