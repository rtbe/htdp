;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 237-238) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 237

; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))

(squared>? 3 10)
(squared>? 4 10)
(squared>? 5 10)

;; Exercise 238

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))

    

; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

; Nelon -> Number
; takes R as comparison function and produces result number
(define (extract R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (R (first l)
            (extract R (rest l)))
         (first l)
         (extract R (rest l)))]))

; Nelon -> Number
; determines the smallest 
; number on l

;(check-expect (inf-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
;     12 11 10 9 8 7 6 5 4 3 2 1)) 1)

;(check-expect (inf-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
;      17 18 19 20 21 22 23 24 25)) 1)

(define (inf-1 l) (extract < l))

; Nelon -> Number
; determines the largest 
; number on l

; (check-expect (sup-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
;      12 11 10 9 8 7 6 5 4 3 2 1)) 25)

; (check-expect (sup-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
;     17 18 19 20 21 22 23 24 25)) 25)

(define (sup-1 l) (extract > l))


; Nelon -> Number
; determines the smallest 
; number on l
(check-expect (inf-min (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1)) 1)

(check-expect (inf-min (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25)) 1)

(define (inf-min l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (inf-min (cons (min (first l) (second l)) (rest (rest l))))]))

; Nelon -> Number
; determines the largest 
; number on l


; Nelon -> Number
; determines the largest 
; number on l

(check-expect (sup-max (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1)) 25)

(check-expect (sup-max (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25)) 25)

(define (sup-max l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (sup-max (cons (max (first l) (second l)) (rest (rest l))))]))

; Nelon -> Number
; extracts number on l by provided function f
(define (extract-f f l) 
  (cond
    [(empty? (rest l)) (first l)]
    [else (extract-f f (cons (f (first l) (second l)) (rest (rest l))))]))


; Nelon -> Number
; determines the smallest 
; number on l

(check-expect (inf-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1)) 1)

(check-expect (inf-2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25)) 1)

(define (inf-2 l) (extract-f min l))


; Nelon -> Number
; determines the largest 
; number on l

(check-expect (sup-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1)) 25)

(check-expect (sup-2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25)) 25)

(define (sup-2 l) (extract-f max l))