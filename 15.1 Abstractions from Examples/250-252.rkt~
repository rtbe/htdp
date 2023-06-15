;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 250-252) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 250


; Number [Number -> Number] -> [List-of Number]
; tabulates provided function between n 
; and 0 (incl.) in a list
(define (tabulate f n)
   (cond
    [(= n 0) (list (f 0))]
    [else
     (cons
      (f n)
      (tabulate f (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tabulate-sqr n) (tabulate sqrt n))

; Number -> [List-of Number]
; tabulates tan between n 
; and 0 (incl.) in a list
(define (tabulate-tan n) (tabulate tan n))


;; Exercise 251

; [List-of Number] [Number -> Number] Number -> Number
; applies provided function f to the every number on l
; n defines return value if a list is empty
(define (fold1 f n l)
  (cond
    [(empty? l) n]
    [else
     (f (first l)
        (fold1 f (rest l)))]))

;; Exercise 252

; [ITEM ITEM2 -> RESULT] RESULT [List-of ITEM] -> RESULT
(define (fold2 f e l)
  (cond
    [(empty? l) e]
    [else
     (f (first l)
        (fold2
         f
         e
         (rest l)))]))
