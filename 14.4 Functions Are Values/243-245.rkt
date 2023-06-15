;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 243

(define (f x) x)

; (cons f '()) - (list f)

; (f f) - function f itself

; (cons f (cons 10 (cons (f 10) '()))) - (list f 10 (f 10))

;; Exercise 244

; (define (f x) (x 10)) - we use f as func and 10 as an argument

; (define (f x) (x f)) - we pass one function as argument to the other

; (define (f x y) (x 'a y 'b)) - x can be a function so exression
; defines function application with the rest of parameters

;; Exercise 245

; Given two functions from numbers to numbers, the function determines
; whether the two produce the same results for 1.2, 3, and -5.775.
(define (function=at-1.2-3-and-5.775? f1 f2)
  (and
   (= (f1 1.2) (f2 1.2))
   (= (f1 3) (f2 3))
   (= (f1 -5.775) (f2 -5.775))))

; We can not hope to define function=?
; because we should test their output on infinite amount of numbers