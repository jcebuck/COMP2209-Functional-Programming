(define-record-type node
  (make-node v l r)
  node?
  (v tree-value)
  (l tree-left)
  (r tree-right))


(define-record-type empty-tree
  (make-empty-tree)
  empty-tree?)


(define (tree-val tree)
  (if (empty-tree? tree)
      '()
      (tree-value tree)))


(define (get-pairs tree)
  (if (empty-tree? tree)
      '()
      (append (list (cons (tree-value tree) (tree-val (tree-left tree))))
              (list (cons (tree-value tree) (tree-val (tree-right tree))))
              (get-pairs (tree-left tree)) (get-pairs (tree-right tree)))))


(define (tree->list tree)
  (let ((pairs (get-pairs tree)))
    (define (set-pairs p)
      (if (null? p)
          '()
          (let ((node (caar p)))
            (if (null? (cdar p))
                (cons (cons node '()) (set-pairs (cddr p)))
                (cons
                 (cons node
                       (map (lambda (x) (cdr x))
                            (filter (lambda (y) (eq? (car y) node)) p)))
                 (set-pairs (filter (lambda (z) (not (eq? (car z) node))) (cdr p))))))))
    (set-pairs pairs)))

(define (tree->list2 tree)
  (if (empty-tree? tree)
      '()
      (foldr (lambda (x l) (cons 
                            (cons (car x)
                                 (if (equal? (cdr x) '(() ()))
                                     '()
                                     (cdr x)))
                            l))
             '()
      (append (list (cons (tree-value tree) (list (tree-val (tree-left tree)) (tree-val (tree-right tree)))))
              (get-pairs (tree-left tree))
              (get-pairs (tree-right tree))))))