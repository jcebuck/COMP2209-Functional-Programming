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


(define make-task cons)
(define task-node car)
(define task-path cdr)
(define same-node? eq?)


(define successors
  (lambda (node graph)
    (map cadddr
         (filter (lambda (statement)
                   (and (eq? (car statement) 'wasDerivedFrom)
                        (eq? (caddr statement) node)))
                 graph))))


(define linked
  (lambda (more-todo select remove avoid-loop)
    (lambda (start end graph)
      
      (define handle-node
        (lambda (node path todo seen)
          (if (same-node? node end)
              (reverse (cons node path))
              (if (avoid-loop node path)
                  (process-todo todo seen)
                  (process-todo (more-todo (map (lambda (successor)
                                                  (make-task successor
                                                             (cons node
                                                                   path)))
                                                (successors node
                                                            graph))
                                           todo)
                                (cons node seen))))))
      
      (define process-todo
        (lambda (todo seen)
          (if (null? todo)
              #f
              (let ((task (select todo)))
                (handle-node (task-node task)
                             (task-path task)
                             (remove task todo)
                             seen)))))
      
      (handle-node  start '() '() '()))))


(define (path-between start end graph)
  ((linked (lambda (x y) (append y x))
           car
           (lambda (x y) (cdr y))
           memq)
   start end graph))