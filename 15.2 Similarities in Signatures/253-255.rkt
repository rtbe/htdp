;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 253-255) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 253

; [Number -> Boolean]
; checks if number is positive
(define (posi? num) (if (num > 0) #t #f))

;; Exercise 254

; [List-of Number] (Number Number -> Boolean) -> [List-of Numbers]
(define (sort-n lon f) ('()))

; [List-of String] (String String -> Boolean) -> [List-of Strings]
(define (sort-s los f) ('()))

; [X] [List-of X] (X X -> Boolean) -> [List-of X]

; [List-of IR] (IR IR -> Boolean) -> [List-of IR]
(define (sort-ir loir f) ('()))

;; Exercise 255

; [List-of Number] (Number -> Number) -> [List-of Number]
(define (map-n lon f) ('()))

; [List-of String] (String -> String) -> [List-of String]
(define (map-s los f) ('()))

; [X] [List-of X] (X -> X) -> [List-of X]
