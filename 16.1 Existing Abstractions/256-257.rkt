;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 256-257) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 256

; finds list with maximum first element
(check-expect (argmax first `((10 11) (9 11) (18 11))) (list 18 11))

; finds list with minimum first element
(check-expect (argmin first `((10 11) (9 11) (18 11))) (list 9 11))

;; Exercise 257

; [X] N [N -> X] -> [List-of X]
; constructs a list by applying f to 0, 1, ..., (sub1 n)
(check-expect (build-l*st 3 add1) (list 1 2 3))

(define (build-l*st n f)
  (cond
    [(= n 0) '()]
    [else (add-at-end (build-l*st (- n 1) f) (f (- n 1)))]))


; [List-of ITEM] ITEM -> [List-of ITEM]
; adds item to the end of l
(check-expect (add-at-end (list 1 2 3) 4) (list 1 2 3 4))

(define (add-at-end l item)
  (cond
    [(empty? l) (cons item '())]
    [else (cons (first l) (add-at-end (rest l) item))]))