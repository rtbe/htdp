;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |142|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; ImageOrFalse is one of:
; – Image
; – #false

; List-of-images is one of:
; - '()
; - (cons Image List-of-images)
(cons (rectangle 20 20 "solid" "red") '())

; List-of-image -> ImageOrFalse
; produces the first image on lon that is not an n by n square
; if it cannot find such an image, it produces #false
(check-expect (ill-sized? (cons (rectangle 20 20 "solid" "red") '()) 20) (rectangle 20 20 "solid" "red"))
(check-expect (ill-sized? (cons (rectangle 20 20 "solid" "red") '()) 30) #false)
(define (ill-sized? lon n)
  (cond
   [(empty? lon) #false]
   [else (if
          (and (= (image-height (first lon)) n) (= (image-width (first lon)) n))
          (first lon)
          (ill-sized? (rest lon) n))]))