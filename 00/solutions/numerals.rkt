#lang racket

(provide from-numeral
         to-numeral
         plus
         mult
         pred)

(define zero (lambda (f v) v))

(define (add1* n)
  (+ 1 n))

(define (succ n)
  (lambda (f v)
    (f (n f v))))

(define (from-numeral n)
  (n add1* 0))

(define (to-numeral n)
  (if (= n 0)
      zero
      (succ (to-numeral (- n 1)))))

(define (plus n m)
  (lambda (f v)
      (m f (n f v))))

(define mult
  (lambda (m n)
    (to-numeral (* (from-numeral n)
                   (from-numeral m)))))

(define pred
  (lambda (n)
    (if (equal? n zero)
        zero
        (to-numeral (-(from-numeral n) 1)))))
