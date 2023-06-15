;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 245

; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; Los -> Boolean
; does l contain "atom"
(define (contains-atom? l)
  (contains? "atom" l))

; Los -> Boolean
; does l contain "basic"
(define (contains-basic? l)
  (contains? "basic" l))

; Los -> Boolean
; does l contain "zoo"
(define (contains-zoo? l)
  (contains? "zoo" l))

;; Exercise 246

(check-expect (add1* (list 1 2 3 4 5)) (list 2 3 4 5 6))

; Lon -> Lon
; adds 1 to each item on l
(define (add1* l) (subs l 1))

; Lon -> Lon
; adds 5 to each item on l

(check-expect (plus5 (list 1 2 3 4 5)) (list 6 7 8 9 10))

(define (plus5 l) (subs l 5))

; Lon -> Lon
; adds n to each item on l
(define (subs l n)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (+ (first l) n)
       (subs (rest l) n))]))
