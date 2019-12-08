#lang racket

(provide winner
         play)

(require "matrix.rkt")
; You can use your matrix functions below, thanks to the "require" invocation above.

(define (id x) x)

(define (x? x) (equal? x "X"))

(define (o? o) (equal? o "O"))

(define (f? f) (equal? f #f))

(define (t? t) (equal? t #t))

(define (win-diag? f xss)
  (or (all? f (car (diags xss)))
      (all? f (car (cdr (diags xss))))))

(define (win-rows-cols? f g xss)
  (any? t? (map (lambda (x) (all? f x)) (g xss))))
  

(define (win? x xss)
  (or (win-diag? x xss)
      (win-rows-cols? x rows xss)
      (win-rows-cols? x cols xss)))
  

(define (winner xss)
  (cond ((win? x? xss) "X")
        ((win? o? xss) "O")
        ((andmap (lambda (xs) (andmap id xs)) xss) "D")
        (else #f)))

; "Dumb" "AI", plays the "next" free spot, going left-to-right, top-to-bottom.
; Put your own implementation here!
(define (play curr-board curr-sign)
  (define (helper i j)
    (cond ((> i 2) #f)
          ((> j 2) (helper (+ i 1) 0))
          ((not (list-ref (list-ref curr-board i) j)) (cons i j))
          (else (helper i (+ j 1)))))
  (helper 0 0))
