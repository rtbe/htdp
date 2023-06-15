;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname htdp) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)

; world constants

(define HEIGHT 200)
(define WIDTH 400)
(define HALF-WIDTH (/ WIDTH 2))
(define V 3)


;graphical constants
(define CIRCLE (overlay (circle 10 "solid" "green")
         (rectangle 40 4 "solid" "green")))
(define CIRCLE-CENTER-TO-TOP
  (- HEIGHT (/ (image-height CIRCLE) 2)))
(define CUSTOM-SCENE (empty-scene WIDTH HEIGHT))


; functions

(define (distance t) (* V t))


(define (dot t)
  (cond
    [(<= (distance t) CIRCLE-CENTER-TO-TOP)
      (place-image CIRCLE
                   HALF-WIDTH (distance t)
                   CUSTOM-SCENE)]
    [(> (distance t) CIRCLE-CENTER-TO-TOP)
      (place-image CIRCLE
                   HALF-WIDTH CIRCLE-CENTER-TO-TOP
                   CUSTOM-SCENE)]))

(animate dot)