(expt (/ (+ 12 8) (- 4 (sqrt 4))) 3)

; 3 - ((8 + 5 ^ 3) + (-7 - 3 * (24 % 7))) ; -> -114

(- 3 (+ (+ 8 (expt 5 3)) (- (- 7) (* 3 (remainder 24 7)))))

(define (sign n)
  (cond ((> n 0) "positive")
        ((< n 0) "negative")
        (else "zero")))

(define (divisible-by? n k)
  (zero? (remainder n k)))

(define (leap? year)
  (or (divisible-by? year 400)
      (and (divisible-by? year 4)
           (not (divisible-by? year 100)))))

(define (^2 x)
  (expt x 2))

(define (area x1 y1 x2 y2 x3 y3)
  (define (distance x1 x2 y1 y2)
    (sqrt (+ (^2 (- x2 x1))
             (^2 (- y2 y1)))))
  (let* ((a (distance x1 x2 y1 y2))
         (b (distance x2 x3 y2 y3))
         (c (distance x3 x1 y3 y1))
         (p (/ (+ a b c) 2)))
    (sqrt (* p (- p a) (- p b) (- p c)))))

(define (sum-n n)
  (if (zero? n) 0
      (+ n (sum-n (- n 1)))))

(define (sum-n* n)
  (define (for i n result)
    (if (> i n) result
        (for (+ i 1) n (+ result i))))
  (for 1 n 0))

(define a 4)

(define 5+
  (lambda (x)
    (+ 5 x)))

(define 3*
  (lambda (x)
    (* 3 x)))

(define (compose g f)
  (lambda (x)
    (g (f x))))

(define (curry f)
  (lambda (x)
    (lambda (y)
      (f x y))))

(define (argmax f a b)
  (define (for i n max-arg)
    (cond ((> i n) max-arg)
          ((> (f i) (f max-arg))
           (for (+ i 1) n i))
          (else (for (+ i 1) n max-arg))))
  (for a b a))

(define (accumulate operation null-value begin end term next)
  (if (> begin end) null-value
      (operation (term begin)
                 (accumulate operation null-value (next begin) end term next))))

(define (sum-odd-squares a b)
  (accumulate + 0 a b
              (lambda (x)
                (if (odd? x)
                    (^2 x) 0))
              ((curry +) 1)))

(define 1+
  ((curry +) 1))

(define (sum-exponents a b)
  (accumulate + 0 a b
              (lambda (c)
                (accumulate + 0 a b ((curry expt) c) 1+))
              1+))





