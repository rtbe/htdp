;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |134|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; A List-of-strings is one of: 
; – '()
; – (cons String List-of-strings)
; interpretation a list of strings


; String List-of-strings -> Boolean
; is a given strung occurs on a given list of strings
(check-expect
  (contains? "A" '())
  #false)
(check-expect
  (contains? "A" (cons "X" (cons "Y"  (cons "Z" '()))))
  #false)
(check-expect
  (contains? "A" (cons "A" (cons "Flatt" (cons "C" '()))))
  #true)
(define (contains? s los)
  (cond
    [(empty? los) #false]
    [(cons? los)
     (if (string=? (first los) s ) #true (contains? s (rest los)))]))