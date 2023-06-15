;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |111|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct vec [x y])
; A vec is
;   (make-vec PositiveNumber PositiveNumber)
; interpretation represents a velocity vector

; Vec -> Vec
; cheks propper inputs for make-vec
(check-expect (checked-make-vec (make-vec 1 1)) true)
(check-expect (checked-make-vec (make-vec -1 0)) (error "x and y must be positive numbers"))
(check-expect (checked-make-vec (make-vec 0 -1)) (error "x and y must be positive numbers"))
(check-expect (checked-make-vec (make-vec -1 -1)) (error "x and y must be positive numbers"))
(define (checked-make-vec v)
  (cond
    [(and
      (and (number? (vec-x v)) (number? (vec-y v)))
      (and (>= (vec-x v) 0) (>= (vec-y v) 0)))
     v]
    [else (error "x and y must be positive numbers")]))
