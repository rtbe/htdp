;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 226-230) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
 
; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes 

;; Exercise 226

; Any -> Boolean
; equality predicate for states

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))


(check-expect (state? '()) #true)
(check-expect (state? 2) #false)
(check-expect (state? fsm-traffic) #true)

(define (state? st)
  (cond
    [(empty? st) #true]
    [(cons? st) (and (transition? (first st))
                     (state? (rest st)))]
    [else #false]))

;; Exercise 227

; is an FSM that flips from black to white and back to black
(define BW-machine
  (list (make-transition "black" "white")
        (make-transition "white" "black")))


(define (simulate.v1 fsm0)
  (big-bang fsm0
    [to-draw render-state.v1]
    [on-key find-next-state.v1]))

; SimulationState.v1 -> Image
; renders a world state as an image 
(define (render-state.v1 s)
  empty-image)
 
; SimulationState.v1 KeyEvent -> SimulationState.v1
; finds the next state from ke and cs
(define (find-next-state.v1 cs ke)
   cs)

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)

; SimulationState.v2 -> Image
; renders a world state as an image 
(define (render-state.v2 s)
  empty-image)
 
; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(define (find-next-state.v2 cs ke)
   cs)

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
 
(check-expect (state-as-colored-square
                (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))
 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from an-fsm and ke

(check-expect
  (find-next-state (make-fs fsm-traffic "red") "n")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "a")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state (make-fs fsm-traffic "green") "q")
  (make-fs fsm-traffic "yellow"))

(define (find-next-state an-fsm ke)
  (make-fs
    (fs-fsm an-fsm)
    (find (fs-fsm an-fsm) (fs-current an-fsm))))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field

(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-error (find fsm-traffic "black")
             "not found: black")

(define (find transitions current)
  (cond
    [(empty? transitions) (error (string-append "not found: " current))]
    [else (if (string=? (transition-current (first transitions)) current)
              (transition-next (first transitions))
              (find (rest transitions) current))]))


;; Exercise 229

; An FSM is one of:
;   – '()
;   – (cons Transition.v2 FSM)

; ExpectsToSee.v1 is one of: 
; – "start, expect an 'a'"
; – "expect 'b', 'c', or 'd'"
; – "finished" 
; – "error, illegal key"

(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")
AA

; ExpectsToSee.v2 is one of:
; – AA
; – BB
; – DD 
; – ER

; FSM-State is a ExpectsToSee.v2

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)

(define regexp (list (make-ktransition AA "a" "BB")
                     (make-ktransition BB "b" "BB")
                     (make-ktransition BB "c" "BB")
                     (make-ktransition BB "d" "DD")))
 
(define (simulate.v3 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square.v3]
    [on-key find-next-state.v3]))

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
 
(check-expect (state-as-colored-square.v3 (make-fs regexp AA))
              (square 100 "solid" "white"))
(check-expect (state-as-colored-square.v3 (make-fs regexp BB))
              (square 100 "solid" "yellow"))
(check-expect (state-as-colored-square.v3 (make-fs regexp DD))
              (square 100 "solid" "green"))
(check-expect (state-as-colored-square.v3 (make-fs regexp ER))
              (square 100 "solid" "red"))
 
(define (state-as-colored-square.v3 an-fsm)
  (cond
    [(string=? (fs-current an-fsm) AA) (square 100 "solid" "white")]
    [(string=? (fs-current an-fsm) BB) (square 100 "solid" "yellow")]
    [(string=? (fs-current an-fsm) DD) (square 100 "solid" "green")]
    [(string=? (fs-current an-fsm) ER) (square 100 "solid" "red")]
    [else (fs-current an-fsm)]))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from an-fsm and ke

(check-expect
  (find-next-state.v3 (make-fs regexp AA) "a")
  (make-fs regexp BB))
(check-expect
  (find-next-state.v3 (make-fs regexp AA) "q")
  (make-fs regexp ER))
(check-expect
  (find-next-state.v3 (make-fs regexp BB) "b")
  (make-fs regexp BB))
(check-expect
  (find-next-state.v3 (make-fs regexp BB) "c")
  (make-fs regexp BB))
(check-expect
  (find-next-state.v3 (make-fs regexp BB) "d")
  (make-fs regexp DD))
(check-expect
  (find-next-state.v3 (make-fs regexp BB) "q")
  (make-fs regexp ER))

(define (find-next-state.v3 an-fsm ke)
  (make-fs
    (fs-fsm an-fsm)
    (find.v3 (fs-fsm an-fsm) (fs-current an-fsm) ke)))

; FSM FSM-State KeyEvent -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field

(check-expect (find.v3 regexp AA "a") BB)
(check-expect (find.v3 regexp AA "q") ER)
(check-expect (find.v3 regexp BB "b") BB)
(check-expect (find.v3 regexp BB "c") BB)
(check-expect (find.v3 regexp BB "d") DD)
(check-expect (find.v3 regexp BB "q") ER)

(define (find.v3 transitions current ke)
  (cond
    [(empty? transitions) ER]
    [else (if (and (string=? (ktransition-current (first transitions)) current)
                   (string=? (ktransition-key (first transitions)) ke))
              (ktransition-next (first transitions))
              (find.v3 (rest transitions) current ke))]))

(simulate.v3 regexp AA)
