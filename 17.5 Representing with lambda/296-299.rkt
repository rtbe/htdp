;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 296-299) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; A Shape is a function: 
;   [Posn -> Boolean]
; interpretation if s is a shape and p a Posn, (s p) 
; produces #true if p is in s, #false otherwise

; Number Number -> Shape 
(define (mk-point x y)
  (lambda (p)
    (and (= (posn-x p) x) (= (posn-y p) y))))
 
(define a-sample-shape (mk-point 3 4))

; Shape Posn -> Boolean
(define (inside? s p)
  (s p))

;; Exercise 297

; Number Number Posn -> Number
; computes the distance between the points (x, y) and p.
(define (distance-between x y p)
  (sqrt (+ (sqr (- x (posn-x p))) (sqr (- y (posn-y p))))))

(define (mk-circle center-x center-y r)
  ; [Posn -> Boolean]
  (lambda (p)
    (<= (distance-between center-x center-y p) r)))

(check-expect
  (inside? (mk-circle 3 4 5) (make-posn 0 0)) #true)
(check-expect
  (inside? (mk-circle 3 4 5) (make-posn 0 9)) #false)
(check-expect
  (inside? (mk-circle 3 4 5) (make-posn -1 3)) #true)


;; Exercise 298

;; Exercise 299

; Set is a function: 
; [Number -> Boolean]
; interpretation produces #true if s is in a set #false otherwise

; representation of a set of all odd numbers
; returns #true if n belongs to a set of all odd numbers
(define odd-set (lambda (n) (if (odd? n) #true #false)))

; representation of a set of all even numbers
; returns #true if n belongs to a set of all even numbers
(define even-set (lambda (n) (if (even? n) #true #false)))

; representation of a set of all numbers divisible by 10
; returns #true if n belongs to a set of all numbers divisible by 10
(define divisible-by-10-set (lambda (n) (= 0 (n 10))))

; Number Set -> Set
; adds an element to a set
(define (add-element n s) (lambda (x) (or (s x) (= x e))))

; Set Set -> Set
; combines the elements of two sets
(define (union s s1) (lambda (x) (or (s x) (s1 x))))

; Set Set -> Set
; collects all elements common to two sets.
(define (intersect s s1)  (lambda (x) (and (s x) (s1 x))))
