;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |94|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define WIDTH 500)
(define HEIGHT 500)
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define UFO-WIDTH (* HEIGHT 1/10))
(define UFO-HEIGHT (* UFO-WIDTH 1/4))
(define UFO (overlay
             (circle UFO-HEIGHT "solid" "green")
             (rectangle UFO-WIDTH 5 "solid" "green")))

(define TANK-WIDTH (* HEIGHT 1/10))
(define TANK-HEIGHT (* TANK-WIDTH 1/3))
(define TANK (rectangle TANK-WIDTH  TANK-HEIGHT "solid" "blue"))

(define MISSLE-SIDE (* WIDTH 1/25))
(define MISSLE (triangle MISSLE-SIDE "solid" "black"))

(place-image
 UFO
 (+ (* 1/2 UFO-WIDTH) 10) 30
 (place-image
  MISSLE
  20 (- HEIGHT 30)
  (place-image
  TANK
  20 (- HEIGHT (* 1/2 TANK-HEIGHT))
  BACKGROUND)))