;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 433-436) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 433
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
; termination (bundle s 0) loops unless s is '()

(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle '() 3) '())
(check-error (bundle '("a" "b") 0))

(define (bundle s n)
  (cond
    [(empty? s) '()]
    [(zero? n) (error "there is infinite loop")]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))

;; Exercise 434

; Comparison operator must be strict < or >.
; Otherwise we will add items equal to pivot, so resulting list will contain pivot again.
; On every iteration of recusion given list will not become smaller then original one

;; Exercise 435

; [List-of Number] [[List-of Number] -> [List-of Number]] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct

(check-expect (quick-sort<.v3 '(11 9 2 18 12 14 4 1) <)
                              '(1 2 4 9 11 12 14 18))

(define (quick-sort<.v3 alon cmp)
  (cond
    [(empty? alon) '()]
    [(empty? (first alon)) (first alon)]
    [else (local ((define pivot (first alon))
                   (define tail (filter (lambda (x) (not (= x pivot))) (rest alon)))) 
            (append (quick-sort<.v3 (filter (lambda (x) (cmp x pivot)) tail) cmp)
                    (list pivot)
                    (quick-sort<.v3 (filter (lambda (x) (not (cmp x pivot))) tail) cmp)))]))

;; Exercise 436
; Did not do exercise 219
