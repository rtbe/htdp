;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |72|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct phone [area switch num])
; A Phone is a structure:
; (make-phone ThreeDigitNumber ThreeDigitNumber FourDigitNumber)
; area is an area code
; switch is a phone switch of neigbourhood
; num is a phone with respect to the neigborhood

; ThreeDigitNumber is a number with only 3 digits
; a Number between 000 and 999 (inclusive)

; FourDigitNumber is a number with only 4 digits
; a Number between 0000 and 9999 (inclusive)