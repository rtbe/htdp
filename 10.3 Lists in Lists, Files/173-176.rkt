;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 173-176) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
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

; LN -> String
; converts a list of lines into a string
; strings is separated by blank spaces (" ")
; lines separated with a newline ("\n")
(check-expect (collapse lls1) "hello world\n\n")
(define (collapse lls)
  (cond
    [(empty? lls) ""]
    [else (string-append
           (collapse-line (first lls)) ; List-of-strings
           (collapse (rest lls)))]))

; List-of-strings -> String
; coverts List-of-strings into a string
; strings separated by blank spaces (" ")
(check-expect (collapse-line line0) "hello world\n")
(check-expect (collapse-line line1) "\n")
(define (collapse-line los) 
  (cond
    [(empty? los) "\n"]
    [else (string-append
           (first los)
           (if (not (empty? (rest los))) " " "")
           (collapse-line (rest los)))]))

(write-file "ttt.dat"
            (collapse (read-words/line "ttt.txt")))

;; Exercise 173

; LN -> LN
; removes articles from provided lls
(define (remove-articles lls)
  (cond
    [(empty? lls) '()]
    [else (cons (remove-articles-ls (first lls)) ; List-of-strings
           (remove-articles (rest lls)))]))

; List-of-strings -> List-of-strings
; removes articles from provided los
(define (remove-articles-ls los)
  (cond
    [(empty? los) '()]
    [else (if (article? (first los))
              (remove-articles-ls (rest los))
              (cons (first los) (remove-articles-ls (rest los))))]))

; String -> Boolean
; defines is a str an arcticle
(check-expect (article? "a") #true)
(check-expect (article? "n") #false)
(define (article? str)
  (cond
    [(string=? str "a") #true]
    [(string=? str "an") #true]
    [(string=? str "the") #true]
    [else #false]))

; String -> File
; consumes name n of a file, reads it,
; and removes all of the arcticles
; and writes result of concatenating "no-articles-" with n

(define (no-articles n)
  (write-file (string-append "no-articles-" n)
              (collapse (remove-articles (read-words/line n)))))

(no-articles "ttt.txt")

; Exercise 174

; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))

; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))

; List-of-1String -> List-of-3string
; encodes strings in provided los
(check-expect (encode-l1s (explode "hello")) (cons "104" (cons "101" (cons"108" (cons "108" (cons "111" '()))))))
(define (encode-l1s l1s)
  (cond
    [(empty? l1s) '()]
    [else (cons
           (encode-letter (first l1s)) ; 1String
           (encode-l1s (rest l1s)))]))

; List-of-3String -> String
; consumes l3s and produces concatenated string
(check-expect (implode-l3s (cons "103" (cons "104" '()))) "103104")
(define (implode-l3s l3s)
  (cond
    [(empty? l3s) ""]
    [else (string-append
           (first l3s) ; 3String
           (implode-l3s (rest l3s)) )]))

; String -> String
; encodes the given string s
(check-expect (encode-string "hello") "104101108108111")
(define (encode-string s)
  (implode-l3s (encode-l1s (explode s))))

; List-of-strings -> List-of-strings
; encodes strings in provided los
(define (encode-los los)
  (cond
   [(empty? los) '()]
   [else (cons
          (encode-string (first los)) ; String
          (encode-los (rest los)))]))

; LN -> LN
; encodes strings in provided lls
(define (encode-lls lls)
  (cond
   [(empty? lls) '()]
   [else (cons
          (encode-los (first lls)) ; List-of-strings
          (encode-lls (rest lls)))]))

; String -> File
; consumes name n of a file, reads it,
; each letter in a word encoded as a numeric three-letter string
; with a value between 0 and 256
(define (encode-file n)
  (write-file (string-append "encoded-" n)
              (collapse (encode-lls (read-words/line n)))))

(encode-file "ttt.txt")

; Exercise 175

; Exercise 176

; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 (cons row1 '()))))

; A List-of-numbers is one of: 
;  – '() 
;  – (cons Number List-of-numbers)

; Matrix -> List-of-numbers
; consumes matrix m and produces the first column as a list of numbers
(check-expect (first* mat1) (cons 11 (cons 21 (cons 11 '()))))
(define (first* m)
  (cond
    [(empty? m) '()]
    [else (cons
           (first** (first m))
           (if (not (empty? (rest m))) (first* (rest m)) '()))]))

; Row -> NumberOrEmptyList
; consumes row r and produces first number
(define (first** r)
  (cond
    [(empty? r) '()]
    [else (first r)]))


; Matrix -> Matrix
; consumes matrix m and removes the first column the result is a matrix
(check-expect (rest* mat1) (cons (cons 12 '()) (cons (cons 11 '()) (cons 12 '()))))
(define (rest* m)
  (cond
    [(empty? (first m) '()]
    [else (cons (rest** (first m)) (rest* (rest m)))]))

; Row -> Row
; consumes row r and removes the first element the result is a row
(define (rest** r)
  (cond
    [(empty? r) '()]
    [else (if
           (not (empty? (rest r)))
           (cons (first (rest r)) (rest** (rest r)))
           (cons (first r) '()))]))



