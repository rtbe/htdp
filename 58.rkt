;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |58|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; A Price falls into one of three intervals: 
; — 0 through 1000
; — 1000 through 10000
; — 10000 and above.
; interpretation the price of an item  

; Price -> Boolean
; decides is p free of taxes
(define (TAX-FREE p)
  (and (<= 0 p) (< p 1000)))

; Price -> Boolean
; decides is p low price
(define (LOW-PRICE p)
  (and (<= 1000 p) (< p 10000)))

; Price -> Boolean
; decides is p a luxury
(define (LUXURY p)
  (>= p 10000))


; Price -> Number
; computes the amount of tax charged for p
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax 1000) (* 0.05 1000))
(check-expect (sales-tax 12017) (* 0.08 12017))

(define (sales-tax p)
  (cond
    [(TAX-FREE p) 0]
    [(LOW-PRICE p) (* 0.05 p)]
    [(LUXURY p) (* 0.08 p)]))