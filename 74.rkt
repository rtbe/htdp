;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |74|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)

(define HEIGHT 400)
(define WIDTH 400)
(define BCKG (empty-scene HEIGHT WIDTH))
(define DOT (circle 10 "solid" "red"))

; Posn -> Image
; places DOT on BCKG according to p
(check-expect (render (make-posn 20 20)) (place-image DOT 20 20 BCKG))
(check-expect (render (make-posn 25 78)) (place-image DOT 25 78 BCKG))
(define (render p)
  (place-image
   DOT
   (posn-x p)
   (posn-y p)
   BCKG))

; Posn Number Number MouseEvt -> Posn
; for mouse clicks, (make-posn x y); otherwise p
(check-expect
  (reset-dot (make-posn 10 20) 29 31 "button-down")
  (make-posn 29 31))
(check-expect
  (reset-dot (make-posn 10 20) 29 31 "button-up")
  (make-posn 10 20))

(define (reset-dot p x y me)
  (cond
    [(mouse=? "button-down" me) (make-posn x y)]
    [(mouse=? "button-up" me) (make-posn (posn-x p) (posn-y p))]
    [else p]))

; Posn -> Posn
; executes every tick
(define (tock p) p)

(define (main initial-state)
  (big-bang initial-state
    [on-tick tock]
    [on-mouse reset-dot]
    [to-draw render]))

(main (make-posn 0 0))