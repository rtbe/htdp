;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 143-148) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; A List-of-temperatures is one of: 
; – '()
; – (cons CTemperature List-of-temperatures)
 
(define ABSOLUTE0 -272)
; A CTemperature is a Number greater than ABSOLUTE0.

; List-of-temperatures -> Number
; computes the average temperature
(check-expect
  (average (cons 1 (cons 2 (cons 3 '())))) 2)

(define (average alot)
  (/ (sum alot) (how-many alot)))
 
; List-of-temperatures -> Number 
; counts the temperatures on the given list 
(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [else (+ (how-many (rest alot)) 1)]))


; List-of-temperatures -> Number 
; adds up the temperatures on the given list 
(define (sum alot)
  (cond
    [(empty? alot) 0]
    [else (+ (first alot) (sum (rest alot)))]))

;; Excercise 143

; (average '()) - "/: division by zero"

; List-of-temperatures -> Number
; computes the average temperature
; if provided list is empty produces error
(check-error (checked-average '()) "alot should not be empty list")
(check-expect
  (checked-average (cons 1 (cons 2 (cons 3 '())))) 2)

(define (checked-average alot)
  (cond
    [(empty? alot) (error "alot should not be empty list")]
    [else (average alot)]))

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures 

;; Exercise 144

; No, it won't. Because both functions work on empty lists (use them as base case)
; but NEList-of-temperatures does not include empty lists in it's definition

;; Exercise 145

; NEList-of-temperatures -> Boolean
; are temperatures sorted in descending order
(check-expect (sorted>? (cons 3 (cons 2 (cons 1 '())))) #true)
(check-expect (sorted>? (cons 2 (cons 1 '()))) #true)
(check-expect (sorted>? (cons 1 (cons 2 (cons 3 '())))) #false)
(check-expect (sorted>? (cons 2 (cons 3 '()))) #false)

(define (sorted>? nlot)
  (cond
   [(empty? (rest nlot)) #true]
   [else (if (> (first nlot) (first (rest nlot))) (sorted>? (rest nlot)) #false)]))

;; Exercise 146

; NEList-of-temperatures -> Number
; counts temperatures on the given list
(check-expect (how-many-nlot (cons 3 (cons 2 (cons 1 '())))) 3)
(check-expect (how-many-nlot (cons 2 (cons 1 '()))) 2)
(check-expect (how-many-nlot (cons 1 '())) 1)

(define (how-many-nlot nlot)
  (cond
    [(empty? (rest nlot)) 1]
    [else (+ 1 (how-many-nlot (rest nlot)))]))

;; Exercise 147

; NEList-of-booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-booleans)
; interpretation non-empty list of booleans
(cons #true '())
(cons #true (cons #false '()))
