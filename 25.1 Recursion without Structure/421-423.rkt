;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 421-423) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time

(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle '() 3) '())

(define (bundle s n)
  (cond
    [(empty? s) '()]
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


;; Exercise 421
; (bundle '("a" "b" "c") 0)
; No it is not a propper use of the bundle.
; It produces nothing, since it blocks the execution due to out of memory error.
; Because it will call the function with the same arguments on each iteration

;; Exercise 422

; [List-of X] Number -> [List-of [List-of X]]
; consumes a list l of arbitrary data and a natural number n

(check-expect (list->chunks (explode "abcdefg") 3)
              (list (list "a" "b" "c") (list "d" "e" "f") (list "g")))
(check-expect (list->chunks '("a" "b") 3) (list (list "a" "b")))
(check-expect (list->chunks '() 3) '())

(define (list->chunks l n)
  (map (lambda (x) (explode x)) (bundle l n)))

(check-expect (list->chunks.v2 (explode "abcdefg") 3)
              (list (list "a" "b" "c") (list "d" "e" "f") (list "g")))
(check-expect (list->chunks.v2 '("a" "b") 3) (list (list "a" "b")))
(check-expect (list->chunks.v2 '() 3) '())

(define (list->chunks.v2 l n)
  (cond
    [(empty? l) '()]
    [else (cons (take l n) (list->chunks.v2 (drop l n) n))]))

(check-expect (bundle.v2 (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (bundle.v2 '("a" "b") 3) (list "ab"))
(check-expect (bundle.v2 '() 3) '())

(define (bundle.v2 s n)
  (map (lambda (x) (implode x)) (list->chunks.v2 s n)))

;; Exercise 423

; String Number -> [List-of String]
; consumes a String s and a natural number n.
; The function produces a list of string chunks of size n

(check-expect (partition "" 1) '())
(check-expect (partition "sas" 1) (list "s" "a" "s"))
(check-expect (partition "sasasa" 3) (list "sas" "asa"))
(check-expect (partition "sasasas" 3) (list "sas" "asa" "s"))
(check-expect (partition "hola manolo" 3) '("hol" "a m" "ano" "lo"))

(define (partition s n)
  (cond
    [(string=? s "") '()]
    [(< (string-length s) n) (cons s '())]
    [else (cons (substring s 0 n) (partition (substring s n) n))]))