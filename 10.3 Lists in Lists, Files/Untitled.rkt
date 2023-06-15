;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)


; List-of-strings is one of:
; - '()
; - (cons String List-of-strings)
; interpretation is a list of strings

; lines
(cons "TTT" (cons "" (cons "Put up in place" (cons "where it's easy to see" '()))))

; words
(cons "TTT" (cons "Put" (cons "up" (cons "in" (cons "place" '())))))


; List-of-list-of-strings (LN) for short is one of:
; - '()
; - (cons List-of-strings LN)


; list-of-list-of-stings

(cons (cons "TTT" '()) (cons "Put" (cons"up" (cons "in" (cons "place" '())))))

; List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)
; interpretation is a list of numbers

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
 
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))

; LN -> List-of-numbers
; determines the number of words on each line
(check-expect (words-on-line lls0) '())
(check-expect (words-on-line lls1)
              (cons 2 (cons 0 '())))

(define (words-on-line ln)
  (cond
    [(empty? ln) '()]
    [else
     (cons (length (first ln))
      (words-on-line (rest ln)))]))

; Exercise 172

; List-of-lines is one of:
; - '()
; - (cons Line List-of-lines)
; interpretation is a list of lines

; Lol -> String
;


