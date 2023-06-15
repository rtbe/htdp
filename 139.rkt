;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |139|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; A List-of-amounts is one of:
; - '()
; (cons PositiveNumber List-of-amounts)
; example: (cons 1 '())

; List-of-amounts -> Number
; computes the sum of amounts
(check-expect (sum '()) 0)
(check-expect (sum (cons 1 '())) 1)
(check-expect (sum (cons 2 (cons 1 '()))) 3)
(define (sum loa)
  (cond
    [(empty? loa) 0]
    [else (+ (first loa) (sum (rest loa)))]))


; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)

; List-of-numbers -> Boolean
; are all numbers in List-of-numbers are positive numbers
(check-expect (pos '()) #true)
(check-expect (pos (cons 5 '())) #true)
(check-expect (pos (cons 10 (cons 5 '()))) #true)
(check-expect (pos (cons -1 '())) #false)
(check-expect (pos (cons -1 (cons 5 '()))) #false)
(check-expect (pos (cons 1 (cons -5 '()))) #false)

(define (pos lon)
  (cond
    [(empty? lon) #true]
    [else (and (> (first lon)  0) (pos (rest lon)))]))


(pos (cons 10 (cons 5 '())))
(pos (cons -1 (cons 5 '())))


(define ERROR "provided lon is not List-of-amounts")
; List-of-numbers -> Number
; produces sum of List-of-numbers if it is List-of-amounts
; otherwise signals an error
(check-error (checked-sum (cons 3 (cons -1 (cons 2 '())))) ERROR)
(check-expect (checked-sum '()) 0)
(check-expect (checked-sum (cons 1 '())) 1)
(check-expect (checked-sum (cons 2 (cons 1 '()))) 3)

(define (checked-sum lon)
  (cond
    [(pos lon) (sum lon)]
    [else (error ERROR)]))