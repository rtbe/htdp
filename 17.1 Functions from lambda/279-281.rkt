;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 279-281) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 279

; 1. Legal

; 2. Legal

; 3. Legal

; 4 Legal

; 5 Illegal

;; Exercise 280

(check-expect ((lambda (x y) (+ x (* x y))) 1 2)
              3)

(check-expect ((lambda (x y)
   (+ x
      (local ((define z (* y y)))
        (+ (* 3 z) (/ 1 x)))))
 1 2)
              14)

(check-expect ((lambda (x y)
   (+ x
      ((lambda (z)
         (+ (* 3 z) (/ 1 z)))
       (* y y))))
 1 2)
              13.25)

;; Exercise 281

; consumes a number and decides whether it is less than 10
((lambda (x) (< x 10)) 4)

; multiplies two given numbers and turns the result into a string
((lambda (x y) (number->string (* x y))) 12 20)

; consumes a natural number and returns 0 for evens and 1 for odds
((lambda (x) (if (odd? x) 1 0)) 1)

(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)

; consumes two inventory records and compares them by price;
((lambda (ir1 ir2) (if (> (IR-price ir1) (IR-price ir2)) #true #false))
 (make-IR "l" 20) (make-IR "s" 10))

; adds a red dot at a given Posn to a given Image.
((lambda (p im) (place-image (circle 5 "solid" "red")
                            (posn-x p)
                            (posn-y p)
                            im))
 (make-posn 25 25) (rectangle 50 50 "solid" "white"))