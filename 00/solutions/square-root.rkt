#lang racket

(provide my-sqrt)

(define (my-sqrt n) 
  (define (helper x e)
    (let ((f (- (expt x 2) n))
          (f* (* x 2)))
      (if (< (abs (/ f f*)) e)
          x
          (helper (- x (/ f f*)) e))))
  (helper (/ n 2.0) 0.0000001))
    
        
        
