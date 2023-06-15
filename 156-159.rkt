;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 156-159) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)

(define HEIGHT 80) ; distances in terms of pixels 
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))
 
; graphical constants 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))

; A Shot is a Number.
; interpretation represents the shot's y-coordinate 

; A List-of-shots is one of: 
; – '()
; – (cons Shot List-of-shots)
; interpretation the collection of shots fired 

; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
;   represents the y-coordinate of a shot

; ShotWorld -> Image
; adds the image of a shot for each y on w 
; at (MID,y) to the background image

(check-expect (to-image '()) BACKGROUND)
(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))
(check-expect (to-image (cons 15 (cons 9 '())))
              (place-image
               SHOT
               XSHOTS 15
               (place-image SHOT XSHOTS 9 BACKGROUND)))


(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else
     (place-image
      SHOT
      XSHOTS (first w)
      (to-image (rest w)))]))


; ShotWorld -> ShotWorld
; moves each shot on w up by one pixel
(check-expect (tock (cons 15 (cons 9 '()))) (cons 14 (cons 8 '())))
(check-expect (tock (cons 9 '())) (cons 8 '()))
(define (tock w)
  (cond
    [(empty? w) '()]
    [else (cons (sub1 (first w)) (tock (rest w)))]))

; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world 
; if the player presses the space bar
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))

;; Exercise 158

; ShotWorld -> ShotWorld
; moves each shot on w up by one pixel
(check-expect (tock.2 (cons 15 (cons 9 '()))) (cons 14 (cons 8 '())))
(check-expect (tock.2 (cons 9 '())) (cons 8 '()))
(define (tock.2 w)
  (cond
    [(empty? w) '()]
    [else (if (>= (sub1 (first w)) 0)
              (cons (sub1 (first w)) (tock (rest w)))
              '())]))

; ShotWorld -> ShotWorld 
; (define (main w0)
; (big-bang w0
;    [on-tick tock.2]
;    [on-key keyh]
;    [to-draw to-image]))

; (main '())


;; Exercise 159

(define-struct pair [balloon# lop])
; A Pair is a structure (make-pair N List-of-posns)
; A List-of-posns is one of: 
; – '()
; – (cons Posn List-of-posns)
; interpretation (make-pair n lob) means n balloons 
; must yet be thrown and added to lop

(define RECTANGLE (rectangle 10 10 "solid" "green"))

; N Image -> Image
;; produces a column - vertical arrangement - of n copies of img
(check-expect (col 0 RECTANGLE) empty-image)
(check-expect (col 1 RECTANGLE) (above RECTANGLE empty-image))
(check-expect (col 3 RECTANGLE) (above RECTANGLE RECTANGLE RECTANGLE empty-image))

(define (col n img)
  (cond
   [(zero? n) empty-image]
   [else (above img (col (sub1 n) img))]))

; N Image -> Image
;; produces a column - vertical arrangement - of n copies of img
(check-expect (row 0 RECTANGLE) empty-image)
(check-expect (row 1 RECTANGLE) (beside RECTANGLE empty-image))
(check-expect (row 3 RECTANGLE) (beside RECTANGLE RECTANGLE RECTANGLE empty-image))

(define (row n img)
  (cond
   [(zero? n) empty-image]
   [else (beside img (row (sub1 n) img))]))

(define LECTURE-HALL (row 10 (col 10 RECTANGLE)))
(define LECTURE-HALL-WIDTH (image-width LECTURE-HALL))
(define LECTURE-HALL-HEIGHT (image-height LECTURE-HALL))

(define LECTURE-HALL-ON-SCENE (place-image
                               LECTURE-HALL
                               (* LECTURE-HALL-WIDTH 1/2) (* LECTURE-HALL-HEIGHT 1/2)
                               (empty-scene (image-width LECTURE-HALL) (image-height  LECTURE-HALL))))

(define BALLOON (circle 5 "solid" "red"))

; -> Posn
; returns posn with random x and y coordinate up to LECTURE-HALL-WIDTH and LECTURE-HALL-HEIGHT
(define RANDOM-POSN (make-posn (random LECTURE-HALL-WIDTH) (random LECTURE-HALL-HEIGHT)))

; N -> List-of-posn
; returns a list of n posn
(define (random-posn-list n)
  (cond
    [(= n 0) '()]
    [else (cons RANDOM-POSN (random-posn-list (sub1 n)))]))
 
; Pair -> Image
; produces an image of lecture hall with red dots added
; as specified by the posns in List-of-posns
(define (add-balloons p)
  (cond
    [(empty? (pair-lop p)) LECTURE-HALL-ON-SCENE]
    [else (place-image
           BALLOON
           (posn-x (first (pair-lop p))) (posn-y (first (pair-lop p)))
           (add-balloons (make-pair (pair-balloon# p) (rest (pair-lop p)))))]))

; Pair -> Pair
; 
(define (tock.b p)
  (cond
    [(= (pair-balloon# p) 0) (make-pair 0 (pair-lop p))]
    [else
     (make-pair
      (sub1 (pair-balloon# p))
      (cons (make-posn (random LECTURE-HALL-WIDTH) (random LECTURE-HALL-HEIGHT)) (pair-lop p)))]))

; N -> Pair 
 (define (main n)
 (big-bang (make-pair n '())
    [on-tick tock.b 1]
    [to-draw add-balloons]))

 (main 5)