;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |47|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)

(define BACKGROUND-WIDTH 800)
(define BACKGROUND-HEIGHT 50)
(define BACKGROUND
  (rectangle (+ BACKGROUND-WIDTH 5) (+ BACKGROUND-HEIGHT 5) "solid" "black"))

; An WorldState is a Number
; interpretation happines level

; WorldState -> WorldState
; decreases cw by 0.1 per tick
(define (clock-tick-handler cw) (if (> cw 100) 100 (- cw 0.1)))

; WorldState -> Image
; renders image of happines gauge
(define (render cw)
  (rectangle cw BACKGROUND-HEIGHT "solid" "red"))

; WorldState -> Boolean
; evaluates after each event
(define (end? cw)(<= cw 0))

; WorldState, String -> WorldState
; changes cw for pressed keys
(define (keyboard-handler cw a-key)
  (cond
    [(key=? a-key "up") (+ cw (* cw 1/3))]
    [(key=? a-key "down") (- cw (* cw 1/5))]
    [else cw]))
  
; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
   (big-bang ws
     [to-draw render]
     [on-tick clock-tick-handler]
     [on-key keyboard-handler]
     [stop-when end?]))

; start with the maximum number of happines
(main 100)