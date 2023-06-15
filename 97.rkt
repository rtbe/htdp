;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |97|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
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

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; Tank Image -> Image
; adds t to the given i
(check-expect (tank-render (make-tank 10 3) BACKGROUND)
              (place-image
               TANK
               10 (- HEIGHT (* 1/2 TANK-HEIGHT))
               BACKGROUND))
(define (tank-render t i)
  (place-image
   TANK
   (tank-loc t) (- HEIGHT (* 1/2 TANK-HEIGHT))
   BACKGROUND))


; UFO Image -> Image
; adds u to the given i
(check-expect (ufo-render (make-posn 10 10) BACKGROUND)
              (place-image
               UFO
               10 10
               BACKGROUND))
(define (ufo-render u i)
  (place-image
   UFO
   (posn-x u) (posn-y u)
   BACKGROUND))


; Missle Image -> Image
; adds m to the given i
(check-expect (missle-render (make-posn 10 10) BACKGROUND)
              (place-image
               MISSLE
               10 10
               BACKGROUND))
(define (missle-render m i) (place-image
                             MISSLE
                             (posn-x m) (posn-y m)
                             BACKGROUND))