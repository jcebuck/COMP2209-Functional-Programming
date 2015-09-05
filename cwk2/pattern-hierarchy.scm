(define statement-id cadr)
(define derivation-src caddr)
(define derivation-dst cadddr)
(define derivation-attrs cddddr)
(define entity-attrs cddr)


(define (is-atom? S)
  (not (pair? S)))


(define (fold-l func acc lst)
  (if (null? lst)
      acc
      (fold-l func
              (func (car lst) acc)
              (cdr lst))))


(define (insert-derivation src dst T)
  (if (null? T)
      (append (list src (list dst)) T)
      (if (is-atom? (car T))
          (if (equal? (car T) src)
              (cons (car T) (append (cdr T) (list (list dst))))
              (cons (car T) (insert-derivation src dst (cdr T))))
          (list (insert-derivation src dst (car T))))))


(define (pattern-hierarchy patterns)
  (fold-l
   (lambda (pattern r)
     (insert-derivation
      (derivation-src pattern)
      (derivation-dst pattern)
      r))
   '()
   patterns))