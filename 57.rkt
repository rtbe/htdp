;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |57|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)

(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
(define YDELTA 3)
 
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
(define CENTER (/ (image-height ROCKET) 2))

(define (BOTTOM-TO-ROCKET x)
  (- HEIGHT CENTER x))

; An LRCD (for launching rocket countdown) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber 
; interpretation a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; bottom of the canvas and the bottom of the rocket

; LRCD -> LRCD
; raises the rocket by YDELTA if it is moving already 
 
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) 0)
(check-expect (fly 10) (+ 10 YDELTA))
(check-expect (fly 22) (+ 22 YDELTA))

(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (+ x 1)]
    [(>= x 0) (+ x YDELTA)]))
 

; LRCD -> Image
; renders the state as a resting or flying rocket

(check-expect
 (show "resting")
 (place-image ROCKET 10 (BOTTOM-TO-ROCKET 0) BACKG))
 
(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              10 (* 3/4 WIDTH)
              (place-image ROCKET 10 (BOTTOM-TO-ROCKET 0) BACKG)))

(check-expect
 (show HEIGHT)
 (place-image ROCKET 10 (BOTTOM-TO-ROCKET HEIGHT) BACKG))
 
(check-expect
 (show 53)
 (place-image ROCKET 10 (BOTTOM-TO-ROCKET 53) BACKG))

(define (show x)
  (cond
    [(string? x) ; resting
     (place-image ROCKET 10 (BOTTOM-TO-ROCKET 0) BACKG)]
    [(<= -3 x -1) ; resting
     (place-image (text (number->string x) 20 "red")
                  10 (* 3/4 WIDTH)
                  (place-image ROCKET
                               10 (BOTTOM-TO-ROCKET 0)
                               BACKG))]
    [(>= x 0) ; flying
     (place-image ROCKET 10 (BOTTOM-TO-ROCKET x) BACKG)]))

; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed, 
; if the rocket is still resting
(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

; LRCD -> Bool
; decides when to stop the world
(define (stop x)
  (cond
    [(string? x) false]
    [else (= x HEIGHT)]))


; LRCD -> LRCD
(define (main2 s)
  (big-bang s
    [to-draw show]
    [on-tick fly 0.01]
    [stop-when stop]
    [on-key launch]))

(main2 "resting")