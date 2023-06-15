;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |98|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define WIDTH 500)
(define HEIGHT 500)
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define UFO-WIDTH (* HEIGHT 1/10))
(define UFO-HEIGHT (* UFO-WIDTH 1/4))
(define UFO (overlay
             (circle UFO-HEIGHT "solid" "green")
             (rectangle UFO-WIDTH 5 "solid" "green")))
(define UFO-DESCEND 3)

(define TANK-WIDTH (* HEIGHT 1/10))
(define TANK-HEIGHT (* TANK-WIDTH 1/3))
(define TANK (rectangle TANK-WIDTH  TANK-HEIGHT "solid" "blue"))

(define MISSILE-SIDE (* WIDTH 1/25))
(define MISSILE (triangle MISSILE-SIDE "solid" "black"))
(define MISSILE-SPEED (* UFO-DESCEND 2))


(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

(define PROXIMITY 5)
; SIGS -> Boolean
; defines condition to stop the world
(define (si-game-over? sigs)
  (cond
    [(aim? sigs) (if (>= (posn-y (aim-ufo sigs)) (- HEIGHT (* 1/2 UFO-HEIGHT)))
                  true
                  false)]
    [(fired? sigs) (if (or
                        (>= (posn-y (aim-ufo sigs)) (- HEIGHT (* 1/2 UFO-HEIGHT)))
                         (and
                          (+ (posn-y (fired-missile sigs)) PROXIMITY) (posn-y (aim-ufo sigs))
                          (>= (posn-x (fired-missile sigs)) (+ (posn-x (fired-ufo sigs) (* 1/2 UFO-WIDTH))))))
                       true
                       false)]))

; SIGS -> Image
; returns final image with a text on BACKGROUND
; - "Earth is doomed" if ufo is able to land on Earth
; - "Earth is saved" if ufo is hitted by the misile
(define (si-render-final sigs)
  (cond
    [(aim? sigs) (place-image (text "Earth is doomed" 20 "red") (* 1/2 WIDTH) (* 1/2 HEIGHT) BACKGROUND)]
    [(fired? sigs) (cond
                     [(>= (posn-y (aim-ufo sigs)) (- HEIGHT (* 1/2 UFO-HEIGHT))) (place-image (text "Earth is doomed" 20 "red") (* 1/2 WIDTH) (* 1/2 HEIGHT) BACKGROUND)]
                     [(and
                          (+ (posn-y (fired-missile sigs)) PROXIMITY) (posn-y (aim-ufo sigs))
                          (>= (posn-x (fired-missile sigs)) (+ (posn-x (fired-ufo sigs) (* 1/2 UFO-WIDTH))))) (place-image (text "Earth is saved" 20 "red") (* 1/2 WIDTH) (* 1/2 HEIGHT) BACKGROUND)])]))


; UFO -> Number
; creates random x-coordinate for the u
(define (ufo-random-x u) (random 5))

; SIGS -> SIGS
; called for every clock tick to determine position of the objects
(define (si-move sigs)
  (cond
    [(aim? sigs) (make-aim
                  (make-posn (ufo-random-x (aim-ufo sigs)) (+ (posn-y (aim-ufo sigs)) UFO-DESCEND)) ; ufo
                  (make-tank (+ (tank-loc (aim-tank sigs)) (tank-vel (aim-tank sigs))) (tank-vel (aim-tank sigs))))] ; tank
   [(fired? sigs) (make-fired
                   (make-posn (ufo-random-x (fired-ufo sigs)) (+ (posn-y (fired-ufo sigs)) UFO-DESCEND)) ; ufo
                   (make-tank (+ (tank-loc (fired-tank sigs)) (tank-vel (fired-tank sigs))) (tank-vel (fired-tank sigs))) ; tank 
                   (make-posn (posn-y (fired-missile sigs)) (+ (posn-y (fired-missile sigs)) MISSILE-SPEED)))])) ; missile