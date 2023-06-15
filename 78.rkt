;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |78|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; OneLetter is a one letter string
; - 1String through "a" to "z"
; - #false

(define-struct three-letter-word [first second third])
; ThreeLetterWord is a structure:
;   (make-three-letter-word "a" false "z")
; interpretation a word that consists of three OneLetter