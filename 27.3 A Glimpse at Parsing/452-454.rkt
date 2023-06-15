;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 452-454) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; File -> [List-of Line]
; converts a file into a list of lines 
(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))
 
; File -> Line
; retrieves the prefix of afile up to the first occurrence of NEWLINE
(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) '()]
    [else (cons (first afile) (first-line (rest afile)))]))
 
; File -> File
; drops the suffix of afile behind the first occurrence of NEWLINE
(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) (rest afile)]
    [else (remove-first-line (rest afile))]))
 
(define NEWLINE "\n") ; the 1String

;; Exercise 452
; Already done

;; Exercise 453

; A Token is one of: 
; - 1String
; - String (lower-case letters and nothing else)

; Line -> [List-of Token]
; turns a Line into a list of tokens

(check-expect (tokenize (explode "say hello to my little friend!"))
              '("say" "hello" "to" "my" "little" "friend!"))

(define (tokenize line)
  (local ((define (word line)
            (cond
              [(empty? line) '()]
              [(string-whitespace? (first line)) '()]
              [else (cons (first line)
                          (word (rest line)))]))
          (define (remove-word line)
            (cond
              [(empty? line) '()]
              [(string-whitespace? (first line)) (rest line)]
              [else (remove-word (rest line))])))
    (cond
      [(empty? line) '()]
      [else (cons (implode (word line)) (tokenize (remove-word line)))])))

;; Exercise 454

(check-expect
  (create-matrix 2 (list 1 2 3 4))
  (list (list 1 2)
        (list 3 4)))

(check-expect
  (create-matrix 4 (list 1 2 3 4))
  (list (list 1 2 3 4)))

; Number [List-of Number] -> [List of [List-of Number]]
; consumes a number n and a list of n2 numbers. It produces an n x n matrix
(define (create-matrix n lon)
  (local ((define (row n lon)
            (cond
              [(= n 0) '()]
              [else (cons (first lon)
                          (row (- n 1) (rest lon)))]))
          (define (remove-row n lon)
            (cond
              [(= n 0) lon]
              [else (remove-row (- n 1) (rest lon))])))
    (cond
      [(empty? lon) '()]
      [(= n 0) '()]
      [else
       (cons (row n lon)
             (create-matrix n (remove-row n lon)))])))


