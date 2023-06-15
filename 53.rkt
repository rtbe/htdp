;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |53|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)

(define HEIGHT 300)
(define WIDTH  100)

(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
(define ROCKET-CENTER (/ (image-height ROCKET) 2)) 


; An LR (short for launching rocket) is one of:
; – "resting"
; – NonnegativeNumber
; interpretation "resting" represents a grounded rocket
; a number denotes the height of a rocket in flight
; given: "resting", expect: (place-image ROCKET (* 1/2 WIDTH) 0 BACKG)
; given: 10, expect: (place-image ROCKET (* 1/2 WIDTH) 10 BACKG)
; given: 30, expect: (place-image ROCKET (* 1/2 WIDTH)) 30 BACKG)
; given: HEIGHT, expect: (place-image ROCKET (* 1/2 WIDTH) HEIGHT BACKG)
(place-image ROCKET (* 1/2 WIDTH) (- 0 ROCKET-CENTER) BACKG)
(place-image ROCKET (* 1/2 WIDTH) (- HEIGHT ROCKET-CENTER) BACKG)
(place-image ROCKET (/ WIDTH 2) 30 BACKG)