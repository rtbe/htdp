;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 154-155) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct layer [color doll])

; An RD (short for Russian doll) is one of: 
; – String 
; – (make-layer String RD)

; RD -> Number
; how many dolls are a part of an-rd
(check-expect (depth "red") 1)
(check-expect
  (depth
   (make-layer "yellow" (make-layer "green" "red")))
  3)

(define (depth an-rd)
  (cond
    [(string? an-rd) 1]
    [else (+ (depth (layer-doll an-rd)) 1)]))

;; Exercise 154

; RD -> String
; produces a string of all colors inside RD, separate by a comma and space
(check-expect
  (colors
   (make-layer "yellow" (make-layer "green" "red")))
  "yellow, green, red")

(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [else (string-append (layer-color an-rd) ", " (colors (layer-doll an-rd)))]))

;; Exercise 155

; RD -> String
; prodices color of the innersmost doll

(check-expect
  (inner
   (make-layer "yellow" (make-layer "green" "red")))
  "red")

(define (inner an-rd)
  (cond
    [(string? an-rd) an-rd]
    [else (inner (layer-doll an-rd))]))