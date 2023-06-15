;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 282-284) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; Exercise 282

; Number -> Boolean
(define (compare x)
  (= (f-plain x) (f-lambda x)))


(define (f-plain x)
  (* 10 x))

(define f-lambda
  (lambda (x)
     (* 10 x)))

; (compare (random 100000))

;; Exercise 283

; (map (lambda (x) (* 10 x))
;      '(1 2 3))

; (foldl (lambda (name rst)
;        (string-append name ", " rst))
;        "etc."
;        '("Matthew" "Robby"))

; (filter (lambda (ir) (<= (IR-price ir) th))
;         (list (make-IR "bear" 10)
;               (make-IR "doll" 33)))

; Exercise 284

; ((lambda (x) x) (lambda (x) x))

; ((lambda (x) (x x)) (lambda (x) x))

((lambda (x) (x x)) (lambda (x) (x x)))

