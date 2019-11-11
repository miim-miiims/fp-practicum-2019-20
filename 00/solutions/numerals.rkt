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

(define (mult m n)
  (m (lambda (v)
       (plus n v)) zero))

(define (pred m)
  (lambda (f v)
    (lambda (n)
      (if (equal? m zero)
          zero
          (if (= (m f v) ((plus n (succ zero))f v))
              n
              (succ n))))
    zero))

            
            

         (define (helper n)
  (from-numeral (pred n)))
        
