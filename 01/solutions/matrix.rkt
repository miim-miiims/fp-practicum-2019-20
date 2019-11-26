#lang racket

(provide all?
         any?
         concat
         rows
         cols
         matrix-ref
         set
         place
         diag
         diags
         map-matrix
         filter-matrix
         zip-with
         zip-matrix)
; the provide "exports" these functions

; 00.
(define (all? p? xs)
  (foldr (lambda(x y) (and x y)) #t (map p? xs)))

; 01.
(define (any? p? xs)
  (not (null? (filter p? xs))))


; 02.
(define (concat xss)
  (foldr append '() xss))

; 03.
(define (rows xss) xss)

; 04.
(define (first-lst xss) (map car xss))

(define (rem-lst xss) (map cdr xss))

(define (cols xss)
  (if(= 1 (length (car xss)))
     (cons (first-lst xss) '())
     (cons (first-lst xss) (cols (rem-lst xss)))))

; 05.
(define (matrix-ref xss i j)
  (list-ref (list-ref xss i) j))

; 06.
(define (set xs i x)
  (if (null? xs)
      '()
      (cons (if (= i 0) x (car xs))
            (set (cdr xs) (- i 1) x))))

; 07.
(define (place xss i j x)
  (if (= i 0)
      (cons (set (car xss) j x) (cdr xss))
      (cons (car xss) (place (cdr xss) (- i 1) j x))))
 
; 08.
(define (frst-el xss) (car (car xss)))

(define (rem-mtrx xss) (map cdr (cdr xss)))

(define (diag xss)
  (if (= 1 (length xss))
      (cons (frst-el xss) '())
      (cons (frst-el xss)
            (diag (rem-mtrx xss)))))
; 09.
(define (flip xss)
  (if (null? xss)
      '()
      (cons (reverse (car xss)) (flip (cdr xss)))))

(define (diags xss)
  (list (diag xss) (diag (flip xss))))

; 10.
(define (map-matrix f xss)
  (map (lambda (x) (map f x)) xss))

; 11.
(define (filter-matrix p? xss)
  (map (lambda (x) (filter p? x)) xss))

; 12.
(define (zip-with f xs ys)
  (if (or (null? xs) (null? ys))
        '()
        (cons (f (car xs) (car ys))
              (zip-with f (cdr xs) (cdr ys)))))
      
; 13.
(define (zip-matrix xss yss)
  (map (lambda (x y) (zip-with cons x y)) xss yss))