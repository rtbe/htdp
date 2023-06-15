;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 149-153) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 149

; I think yes, copier function works propperly with other values

(define (copier n s)
  (cond
    [(zero? n) '()]
    [(positive? n) (cons s (copier (sub1 n) s))]))

(copier 4 #false)
(copier 3 (rectangle 20 20 "solid" "red"))

;; Exercise 150

; N -> Number
; computes (+n pi) without using + operator

(check-within (add-to-pi 3) (+ 3 pi) 0.001)
(check-within (add-to-pi 5) (+ 5 pi) 0.001)
(check-within (add-to-pi 0) (+ 0 pi) 0.001)
 
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

; N Number -> Number
; computes (+n x) wihout using + operator

(check-expect (add 3 3) 6)
(check-expect (add 3 0) 3)

(define (add n x)
  (cond
   [(zero? n) x]
   [else (add1 (add (sub1 n) x))]))

;; Exercise 151

; N Number -> Number
; computes (* n x) without using * operator

(check-expect (multiply 3 0) 0)
(check-expect (multiply 3 1) 3)
(check-expect (multiply 3 3) 9)

(define (multiply n x)
  (cond
   [(zero? n) 0]
   [else (+ x (multiply (- n 1) x))]))

;; Exercise 152

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

;; Exercise 153

;; List-of-posns is one of:
;; - '()
;; - (cons Posn List-of-posns)
(cons (make-posn 10 10) '())
(cons (make-posn 20 20) (cons (make-posn 10 10) '()))

(define LECTURE-HALL (row 10 (col 10 RECTANGLE)))

(define LECTURE-HALL-ON-SCENE (place-image
                               LECTURE-HALL
                               (* (image-width LECTURE-HALL) 1/2) (* (image-height LECTURE-HALL) 1/2)
                               (empty-scene (image-width LECTURE-HALL) (image-height  LECTURE-HALL))))

(define BALLOON (circle 5 "solid" "red"))
  
; List-of-Posn -> Image
; produces an image of lecture hall with red dors added
; as specified by the posns in List-of-posns

(define (add-balloons lop)
  (cond
    [(empty? lop) LECTURE-HALL-ON-SCENE]
    [else (place-image
           BALLOON
           (posn-x (first lop)) (posn-y (first lop))
           (add-balloons (rest lop)))]))

(add-balloons (cons (make-posn 40 43) (cons (make-posn 20 20) (cons (make-posn 10 10) '()))))