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
  (if (null? xs)
      #t
      (and (p? (car xs))
           (all? p? (cdr xs)))))

; 01.
(define (any? p? xs)
  (not (null? (filter p? xs))))


; 02.
(define (concat xss)
  (foldr append '() xss))

; 03.
(define (rows xss) xss)

; 04.
(define (cols xss)
  (apply map list xss))

; 05.
(define (matrix-ref xss i j)
  (cond ((null? xss) -1)
        ((= i 0) (list-ref (car xss) j))
        (else (matrix-ref (cdr xss) (- i 1) j))))

; 06.
(define (set xs i x)
  (if (null? xs)
      '()
      (cons (if (= i 0) x (car xs))
            (set (cdr xs) (- i 1) x))))

; 07.
(define (place xss i j x)
   (if (null? xss)
      '()
      (cons (if (= i 0)
                (set (car xss) j x)
                (car xss))
            (place (cdr xss) (- i 1) j x))))

; 08.
(define (diag xss)
  (define (helper n)
    (if (= n (length (car xss)))
        '()
        (cons (matrix-ref xss n n)  (helper (+ n 1)))))
  (helper 0))

(define (flip xss)
  (if (null? xss)
      '()
      (cons (reverse (car xss)) (flip (cdr xss)))))

; 09.
(define (diags xss)
  (list (diag xss) (diag (flip xss))))

; 10.
(define (map-matrix f xss)
  (map (lambda(x) (map f x)) xss))

; 11.
(define (filter-matrix p? xss)
  (map (lambda(x) (filter p? x)) xss))

; 12.
(define (zip-with f xs ys)
  (define (helper n xs ys)
    (if (= 0 (min (length xs) (length ys)))
        '()
        (cons (f (car xs) (car ys))
              (helper (+ n 1) (cdr xs) (cdr ys)))))
  (helper 0 xs ys))

; 13.
(define (zip-matrix xss yss)
  (if (or (null? xss) (null? yss))
      '()
      (cons (zip-with cons (car xss) (car yss))
            (zip-matrix (cdr xss) (cdr yss)))))
 