;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |64|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))


; computes the distance of a point to the origin considers a path
; that follows the rectangular grid of streets found in Manhattan
(define (manhattan-distance ap)
    (+ (posn-x ap)
       (posn-y ap)))

(manhattan-distance (make-posn 4 3))

(manhattan-distance (make-posn 6 (* 2 4)))

(+ (manhattan-distance (make-posn 12 5)) 10)