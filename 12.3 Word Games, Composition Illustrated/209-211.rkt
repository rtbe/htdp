;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)

; On OS X: 
(define LOCATION "/usr/share/dict/words")
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
; On WINDOWS: borrow the word file from your Linux friend
 
; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

;; Exercise 209

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; String -> Word
; converts s to the chosen word representation

(check-expect (string->word "loh") (list "l" "o" "h"))

(define (string->word s) (explode s))
 
; Word -> String
; converts w to a string

(check-expect (word->string (list "l" "o" "h")) "loh")

(define (word->string w) (implode w))

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))

; Word -> List-of-words
; finds all rearrangements of word
(define (arrangements word)
  (list word))

; String -> List-of-strings
; finds all words that the letters of some given word spell
 
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
 
(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))
 

;; Exercise 210

; List-of-Words -> List-of-Strings
; returns List-of-Strings from provided List-of-Words

(check-expect (words->strings (list (list "t" "y") (list "l" "o" "h"))) (list "ty" "loh"))

(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low)) (words->strings (rest low)))]))


;; Exercise 211

; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary

(check-expect (in-dictionary (list "loh" "a" "vdv")) (list "a"))

(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [else (if
           (member? (first los) AS-LIST)
           (cons (first los) (in-dictionary (rest los)))
           (in-dictionary (rest los)))]))