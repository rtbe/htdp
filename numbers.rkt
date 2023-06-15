;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname numbers) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)
;
(define prefix "hello")
(define suffix "world")

(string-append prefix "_" suffix)
"==========================================================="
;
(define str "helloworld")
(define i 5)
(define str-length (string-length str))

(define (del pos)
  (cond
        [(< pos 0) "pos should be positive"]
        [(> (+ pos 1) str-length) "pos is bigger than str length"]
                        ))

(del -1)
"==========================================================="
; exercise 9
(define in 1)

(cond
  [(string? in) (string-length in)]
  [(image? in) (* (image-height in) (image-width in))]
  [(number? in) in]
  [(not (false? in)) 10]
  [(false? in) 20]
  )
"==========================================================="
; exercise 10
"==========================================================="
; exercise 11
(define (distance x y) (sqrt (+ (sqr x) (sqr y))))

(distance 5 5)
"==========================================================="
; exercise 12
(define (cvolume sideLength)
  (expt sideLength 3))

(cvolume 3)
"==========================================================="
; exercise 13
(define (string-first str)
  (if (string? str)
      (if (> (string-length str) 0)
          (string-ith str 0)
          "str shold not be empty")
      "str should be string"))

(string-first "loh")
(string-first "")
(string-first 1)
"==========================================================="
; exercise 14
(define (string-last str)
  (if (string? str)
      (if (> (string-length str) 0)
          (string-ith str (- (string-length str) 1))
          "str should be not empty")
      "str shold be string"))

(string-last "loh")
(string-last "")
(string-last 1)
"==========================================================="
; exercise 15
(define (==> sunny friday)
  (if (and (false? sunny) (not (false? friday))) true false))
(==> false true)
(==> true false)
(==> true true)
(==> false false)
"==========================================================="
; exercise 16
(define (image-area img)
  (* (image-height img) (image-width img)))
(image-area (circle 10 "solid" "black"))
"==========================================================="
; exercise 17
(define (image-classify img)
  (cond
   [(not(image? img)) "img should be image"]
   [(= (image-height img) (image-width img)) "square"]
   [(> (image-height img) (image-width img)) "tall"]
   [(< (image-height img) (image-width img)) "wide"]))

(image-classify (circle 10 "solid" "black"))
(image-classify 1)
"==========================================================="
; exercise 18
(define (string-join str1 str2)
  (if (and (string? str1) (string? str2))
      (string-append str1 "_" str2)
      "str1 and str2 should be strings"))


(string-join 1 2)
(string-join "loh" "pidr")
"==========================================================="
; exercise 19
(define (string-insert str num)
  (cond
    [(not (string? str)) "str should be string"]
    [(= (string-length str) 0) "str should not be empty"]
    [(or (< num 0) (> num (string-length str))) "str should be between 0 and length of str"]
    [(and (>= num 0) (<= num (string-length str)))
     (string-append (substring str 0 num) "_" (substring str num))]))

(string-insert 1 1)
(string-insert "" 1)
(string-insert "loh" 0)
(string-insert "loh" 1)
(string-insert "loh" 2)
(string-insert "loh" 3)
(string-insert "loh" 4)
"==========================================================="
; exercise 20
(define (string-delete str num)
  (cond
    [(not (string? str)) "str should be string"]
    [(= (string-length str) 0) "str should not be empty"]
    [(or (< num 0) (>= num (string-length str))) "str should be between 0 and length of str - 1"]
    [(and (>= num 0) (< num (string-length str)))
     (string-append (substring str 0 num) (substring str (+ num 1)))]))

(string-delete 1 1)
(string-delete "" 1)
(string-delete "loh" 0)
(string-delete "loh" 1)
(string-delete "loh" 2)
(string-delete "loh" 3)
(string-delete "loh" 4)
"==========================================================="
; exercise 21
; exercise 22
; exercise 23
; exercise 24
; exercise 25
; exercise 26
; exercise 27
(define PEOPLE 120)
(define FIXED-COST 0)
(define ATTENDEE-COST 1.50)
(define PRICE-SENSITIVITY (/ 15 0.1))

(define (attendees ticket-price)
  (- PEOPLE (* (- ticket-price 5.0) PRICE-SENSITIVITY)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ FIXED-COST (* ATTENDEE-COST (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))
"==========================================================="
; exercise 28
(profit 1)
(profit 2)
(profit 3)
(profit 4)
(profit 5)
"==========================================================="
; exercise 29
(profit 3)
(profit 4)
(profit 5)
"====================[exercise 30]===================="
"====================[exercise 31]===================="
"====================[exercise 32]===================="
"====================[exercise 33]===================="
"====================[exercise 34]===================="
; String -> String
; return first character from str
; given: "test", expect: "t"
; +exercise 13
(string-first "test")
"====================[exercise 35]===================="
; exercise 35
; String -> String
; return last character from str
; given: "test", expect: "t"
; +exercise 14
(string-last "test")
"====================[exercise 36]===================="
; Image -> Number
; count the number of pixels in img
; given: (square 10 "solid" "red"), expect: 100
; +exercise 15
(image-area (square 10 "solid" "red"))
"====================[exercise 38]===================="
; String -> String
; return a string with the first character removed
; given: "test", expect: "est"
(define (string-rest str)
  (substring str 1))

(string-rest "test")
"====================[exercise 38]===================="
; String -> String
; return a string without last character
; given: "test", expect: "tes"
(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))

(string-remove-last "test")
"====================[exercise 39]===================="
(define WHEEL-RADIUS 5)

(define WHEEL-DIAMETER (/ WHEEL-RADIUS 2))

(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))

(define SPACE
  (rectangle (* 5 WHEEL-RADIUS) WHEEL-DIAMETER  "solid" "white"))

(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))

(define CAR-BODY
  (rectangle (* 8 WHEEL-RADIUS) (* 4 WHEEL-RADIUS) "solid" "red"))

(define CAR
  (underlay/xy
   CAR-BODY
   0 (- (image-height CAR-BODY) WHEEL-DIAMETER)
   BOTH-WHEELS))
"====================[exercise 40]===================="
; WorldState -> WorldState
; moves the car by 3 pixels per clock tick
(check-expect (tock 3) 6)
(check-expect (tock 11) 14)
(define (tock cw) (+ cw 3))
"====================[exercise 41]===================="
(define tree
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))

(define BACKGROUND-WIDTH (* 100 WHEEL-RADIUS))
(define BACKGROUND-HEIGHT (* 8 WHEEL-RADIUS))
(define BACKGROUND
  (place-image
     tree
     (* 50 WHEEL-RADIUS)
     (* 4 WHEEL-RADIUS)
     (rectangle BACKGROUND-WIDTH  BACKGROUND-HEIGHT  "solid" "white")))

(define Y-CAR (- (image-height BACKGROUND) (image-height CAR)))

; WorldState -> Image
; places the car into BACKGROUND scene,
; according to the given world state
(define (render cw)
  (place-image CAR cw Y-CAR BACKGROUND))

; WorldState -> Boolean
; after each event, big-bang evaluated (end? cw)
(define (end? cw) (>= cw (+ 5 BACKGROUND-WIDTH (* 1/2 (image-width CAR)))))

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
   (big-bang ws
     [on-tick tock]
     [to-draw render]
     [stop-when end?]))

(main 0)
"====================[exercise 42 SKIPPED]===================="
"====================[exercise 43 SKIPPED]===================="
"====================[exercise 44 SKIPPED]===================="

