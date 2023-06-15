;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 378-386) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)


;; Exercise 378

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons FSM-State (cons FSM-State '()))
; An FSM-State is a String that specifies a color
 
; data examples 
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))
 
; FSM-State FSM -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
       (overlay (text current 16 "black") (square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (find transitions current))]))
 
; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist

(check-expect (find fsm-traffic "green") "yellow")
(check-error (find fsm-traffic "black"))

(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

; (simulate "green" fsm-traffic)

;; Exercise 380

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons FSM-State (cons (cons FSM-State (cons Key-Event '())) '()))

(define fsm-traffic.v2
  '((("red" "g") "green") (("green" "y") "yellow") (("yellow" "r") "red")))

; FSM-State FSM -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate.v2 state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
       (overlay (text current 16 "black") (square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (find transitions (list current key-event)))]))

; (simulate.v2 "green" fsm-traffic.v2)

; Exercise 381

; An XMachine is a nested list of this shape:
;   (list 'machine (list (list 'initial FSM-State)  [List-of X1T]))
; An X1T is a nested list of this shape:
;   (list 'action (list (list 'state FSM-State) (list 'next FSM-State)))

;; Exercise 382

; <machine initial="white">
;  <action state="white"  next="black" />
;  <action state="black"  next="white" />
; </machine>

(define bw
  '(machine ((initial "white"))
     (action ((state "white") (next "black")))
     (action ((state "black") (next "white")))))

;; Exercise 383
