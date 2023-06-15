;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 161-165) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)
; interpretation is a list of numbers


;; Exercise 161
; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(check-expect (wage* (cons 28 '())) (cons 336 '()))
(check-expect (wage* (cons 4 (cons 2 '()))) (cons 48 (cons 24 '())))

(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))
 
; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* 12 (<100? h)))

; Number -> Number
; returns an error if h is more than 100 if not returns #true
(define (<100? h) (if (< 100 h) (error "h is more than 100") h))

;; Exercise 163
; takes list of measurements in Fahrenheit
; and returns a list of measurements in Celsius
(check-expect (convertFC (cons -40 '())) (cons -40 '()))
(check-expect (convertFC (cons 32 '())) (cons 0 '()))
(define (convertFC lof)
  (cond
    [(empty? lof) '()]
    [else (cons (* (- (first lof) 32) 5/9) (convertFC (rest lof)))]))

;; Exercise 164
; List-of-numbers -> List-of-numbers
; converts list of US$ amounts into list of € amounts
(check-expect (convert-euro (cons 1 '())) (cons (/ 101 100) '()))
(define (convert-euro lod)
  (cond
    [(empty? lod) '()]
    [else (cons (* (first lod) (/ 101 100)) (convert-euro (rest lod)))]))

; List-of-numbers -> List-of-numbers
; converts list of US$ amounts into list of € amounts
; by provided exchange rate rate
(check-expect (convert-euro* (cons 1 '()) (/ 101 100)) (cons (/ 101 100) '()))
(define (convert-euro* lod rate)
  (cond
    [(empty? lod) '()]
    [else (cons (* (first lod) rate) (convert-euro (rest lod)))]))

;; Exercise 165

; List-of-strings is one of:
; - '()
; - (cons String List-of-strings)
; interpretation is a list of strings

; List-of-strings -> List-of-strings
; replaces all ocurrences of "robot" with "r2d2" in provided los
(check-expect (subst-robot (cons "robot" (cons "robot" '()))) (cons "r2d2" (cons "r2d2" '())))
(check-expect (subst-robot (cons "r2d2" (cons "r2d2" '()))) (cons "r2d2" (cons "r2d2" '())))
(define (subst-robot los)
  (cond
    [(empty? los) los]
    [else (cons (...) "r2d2" (first los)) (subst-robot (rest los)))]))

; List-of-strings, String, String -> List-of-strings
; replaces all ocurrences of old with new
(check-expect (substitute "r2d2" "robot" (cons "robot" (cons "robot" '()))) (cons "r2d2" (cons "r2d2" '())))
(check-expect (substitute "r2d2" "robot" (cons "r2d2" (cons "r2d2" '()))) (cons "r2d2" (cons "r2d2" '())))
(define (substitute new old los)
  (cond
    [(empty? los) los]
    [else (cons (if (string=? (first los) old) new (first los)) (substitute new old (rest los)))]))
