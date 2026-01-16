(define pair (cons 2 4))

pair
(car pair)
(cdr pair)
(define nested (cons 3 pair))
nested
(cadr nested)
(cddr nested)

(define l (cons 1 (cons 2 (cons 3 '()))))
l

(define (foldr operation null_value lst)
  (if (null? lst) null_value
      (operation (car lst)
          (foldr operation null_value (cdr lst)))))

(define (foldl operation null_value lst)
  (if (null? lst) null_value
      (foldl operation (operation null_value (car lst)) (cdr lst))))

(define (filter pred? lst)
  (foldr (lambda (current result)
           (if (pred? current)
               (cons current result)
               result)) '() lst))

(define (find lst pred?)
  (cond ((null? lst) #f)
        ((pred? (car lst)) (car lst))
        (else (find (cdr lst) pred?))))
  
(define (zip lst1 lst2)
  (if (or (null? lst1)
          (null? lst2))
      '()
      (cons (cons (car lst1)
                  (car lst2))
            (zip (cdr lst1)
                 (cdr lst2)))))

(define (max-repeated lst)
  (define (max-repeated-helper max-n current-n last lst)
    (cond ((null? lst) (max max-n current-n))
          ((equal? last (car lst))
           (max-repeated-helper max-n (+ current-n 1) last (cdr lst)))
          (else (max-repeated-helper (max max-n current-n) 1 (car lst) (cdr lst)))))
  (if (null? lst) 0
      (max-repeated-helper 1 1 (car lst) (cdr lst))))


(define (intersection lst1 lst2)
  (filter (lambda (x) (member x lst2)) lst1))

(define (union lst1 lst2)
  (let ((diff (filter (lambda (x) (not (member x lst1))) lst2)))
    (append lst1 diff)))

(define (min-l lst)
  (apply min lst))

(define (average a . lst)
  (/ (apply + a lst)
     (+ 1 (length lst))))

(define (|| a b)
  (or a b))

(define (any? pred? lst)
  (foldr || #f (map pred? lst)))

(define (map-var f . args)
 (if (any? null? args) '()
     (cons (apply f (map car args))
           (apply map-var f (map cdr args)))))

(define a (delay 4))
(force a)

(cons 3 (lambda () (/ 7 0)))

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))

(define head car)
(define (tail s) (force (cdr s))) 

(define (from n)
  (cons-stream n (from (+ n 1))))

(define nats (from 0))

(define (take n stream)
  (if (or (<= n 0)
          (null? stream))
      '()
      (cons (head stream)
            (take (- n 1) (tail stream)))))

(define (map-stream f l)
  (if (null? l) '()
      (cons-stream (f (head l))
                   (map-stream f (tail l)))))

(define nats-m
  (cons-stream 0 (map-stream (lambda (x) (+ x 1)) nats-m)))

(define (zip-with f lst1 lst2)
  (if (or (null? lst1)
          (null? lst2))
      '()
      (cons-stream (f (head lst1)
                      (head lst2))
                   (zip-with f (tail lst1)
                             (tail lst2)))))

(define fib
  (cons-stream 0 (cons-stream 1
      (zip-with + fib (tail fib)))))

(define a 5)

(define (f)
  (begin
    (display a)
    (display "\n")
    (newline)
    (set! a 3)
    (display a)))














  



