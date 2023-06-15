;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 437-) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 437

; [List-of X] -> Number
; computes the length of its input

(check-expect (special-length '()) 0)
(check-expect (special-length '(1 2 3)) 3)

(define (special-length l)
  (cond
    [(empty? l) 0]
    [else (+ 1 (special-length (rest l)))]))

; [List-of Number] -> [List-of Number]
; negates each number on the given list of numbers

(check-expect (special-negate '(1 0 -3)) '(-1 0 3))

(define (special-negate l) (map (lambda (x) (* x -1)) l))

; [List-of String] -> [List-of String]
; special uppercases the given list of strings

(check-expect (special-upper '("lol" "pk")) '("LOL" "PK"))

(define (special-upper l) (map (lambda (x) (string-upcase x)) l))

; Generative and structural recursion are pretty much the same,
; but generative one is more abstract (general)


