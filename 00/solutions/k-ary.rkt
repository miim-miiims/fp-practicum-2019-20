#lang racket

(provide from-k-ary
         to-k-ary)

(define (add1* x)
  (+ x 1))

(define (multiply n k step counter)
  (* (remainder n k)
     (expt step counter)))

(define (from-k-ary n k)
  (define (helper n k acc counter)
    (if (= n 0)
        acc
        (helper (quotient n 10) k
                (+ acc (multiply n 10 k counter))
                (add1 counter))))
    (helper n k 0 0))

(define (to-k-ary n k)
  (define (helper n k acc counter)
    (if (= n 0)
        acc
        (helper (quotient n k) k
                (+ acc (multiply n k 10 counter))
                (add1 counter))))
  (helper n k 0 0))
