;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 239

; A [List X Y] is a list: 
;   (cons X (cons Y '()))

; A [List Number Number] is a list of numbers
; (cons 1 (cons 2 '())

; A [List Number 1String] is a list of number and 1string
; (cons 1 (cons "s" '())

; A [List String String] is a list of strings
; (cons "qwe" (cons "rty" '())

; A [List String Boolean] is a list of string and boolean 
; (cons "qwer" (cons #true '())


;; Exercise 240

(define-struct layer [stuff])

; A [L S] is a structure:
; (make-layer S)

; AN [L Number] is one of:
; - Number
; - (make-layer Number)

; AN [L Str] is one of:
; - String
; - (make-layer String)

;; Exercise 241

; A [NEList-of ITEM] is one of:
; - (cons ITEM '())
; - (cons ITEM [NEList-of ITEM])


;; Exercise 242

; An [Maybe String] is a one of:
; - #false
; - String

; An [Maybe [List-of String]] is one of:
; - #false
; - [List-of String]

; An [List-of [Maybe String]] is one of:
; - '()
; - (cons [Maybe String] [List-of [Maybe String]])

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise 
(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(define (occurs s los)
  (cond
   [(empty? los) #false]
   [else (if
          (string=? s (first los))
          (rest los)
          (occurs s (rest los)))]))



