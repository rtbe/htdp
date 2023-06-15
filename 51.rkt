;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |51|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)

; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three 
; possible states that a traffic light may assume

; TrafficLight -> Image
; returns circle with diameter of 10 and colour of c
(define (traffic-light c)(circle 10  "solid" c))

; TrafficLight -> TrafficLight
; defines next TrafficLight from given TrafficLight
(define (traffic-light-next s)
  (cond
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]
    [(string=? "red" s) "green"]))

; WorldState -> WorldState
; increments cw by 1 per tick
(check-expect (tock 1) 2)
(check-expect (tock 11) 12)
(define (tock cw) (+ 1 cw))

(modulo 2 3)

; WorldState -> Image
; renders next state of traffic-light
(define (render cw)
  (cond
   [(= (modulo cw 3) 0) (traffic-light (traffic-light-next "red"))]
   [(= (modulo cw 3) 2) (traffic-light (traffic-light-next "yellow"))]
   [(= (modulo cw 3) 1) (traffic-light (traffic-light-next "green"))]))


; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
   (big-bang ws
     [on-tick tock]
     [to-draw render]))

(main 0)