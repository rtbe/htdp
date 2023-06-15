;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 424-430) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct

(check-expect (quick-sort< '(11 9 2 18 12 14 4 1))
                           '(1 2 4 9 11 12 14 18))

(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))


;; Exercise 424
; Did on list of paper by hand

;; Exercise 425

; [List-of Number] Number -> [List-of Number]
; consumes list of numbers alon and number n
; returns a list of numbers from alon with numbers larger than n 
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))

; [List-of Number] Number -> [List-of Number]
; consumes list of numbers alon and number n
; returns a list of numbers from alon with numbers smaller than n
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

;; Exercise 426

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct

(check-expect (quick-sort<.v2 '(11 9 2 18 12 14 4 1))
                              '(1 2 4 9 11 12 14 18))

(define (quick-sort<.v2 alon)
  (cond
    [(empty? alon) '()]
    [(empty? (first alon)) (first alon)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort<.v2 (smallers alon pivot))
                    (list pivot)
                    (quick-sort<.v2 (largers alon pivot))))]))

; For every single item in the list it is saves 2 function calls

;; Exercise 427

; List-of-numbers -> List-of-numbers
; produces a sorted version of l

(check-expect (sort< '()) '())
(check-expect (sort< (list 1 2 3)) (list 1 2 3))
(check-expect (sort< (list 3 2 1)) (list 1 2 3))
(check-expect (sort< (list 12 20 -5))
              (list -5 12 20))

(define (sort< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort< (rest l)))]))

(check-expect (quick-sort<.v2* '(11 9 2 18 12 14 4 1))
                              '(1 2 4 9 11 12 14 18))

(define (quick-sort<.v2* alon)
  (cond
    [(empty? alon) '()]
    [(empty? (first alon)) (first alon)]
    [(< (length alon) 20) (sort< alon)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort<.v2* (smallers alon pivot))
                    (list pivot)
                    (quick-sort<.v2* (largers alon pivot))))]))

 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l

(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 5 6))
(check-expect (insert 5 (list 4)) (list 4 5))

(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (< n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))


;; Exercise 428

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct

(check-expect (quick-sort<.v2*** '(11 11 9 2 18 12 14 4 1))
                                 '(1 2 4 9 11 11 12 14 18))
(check-expect (quick-sort<.v2*** '(11 9 9 9 2 18 12 14 4 1))
                                 '(1 2 4 9 9 9 11 12 14 18))

(define (quick-sort<.v2*** alon)
  (cond
    [(empty? alon) '()]
    [(empty? (first alon)) (first alon)]
    [else (local ((define pivot (first alon))
                  (define (how-many-pivot alon)
                    (cond
                      [(empty? alon) 0]
                      [else (+ (if (= pivot (first alon)) 1 0)
                               (how-many-pivot (rest alon)))])))
            
            (append (quick-sort<.v2*** (smallers alon pivot))
                    (make-list (how-many-pivot alon) pivot)
                    (quick-sort<.v2*** (largers alon pivot))))]))


;; Exercise 429

; [List-of Number] Number -> [List-of Number]
; consumes list of numbers alon and number n
; returns a list of numbers from alon with numbers larger than n 
(define (largers.v2 alon n)
  (filter (lambda (x) (> x n)) alon))

; [List-of Number] Number -> [List-of Number]
; consumes list of numbers alon and number n
; returns a list of numbers from alon with numbers smaller than n
(define (smallers.v2 alon n)
  (filter (lambda (x) (< x n)) alon))

(check-expect (quick-sort<.v2**** '(11 9 2 18 12 14 4 1))
                                  '(1 2 4 9 11 12 14 18))

(define (quick-sort<.v2**** alon)
  (cond
    [(empty? alon) '()]
    [(empty? (first alon)) (first alon)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort<.v2**** (smallers.v2 alon pivot))
                    (list pivot)
                    (quick-sort<.v2**** (largers.v2 alon pivot))))]))


;; Exercise 430

; [List-of Number] [[List-of Number] -> [List-of Number]] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct

(check-expect (quick-sort<.v3 '(11 9 2 18 12 14 4 1) <)
                              '(1 2 4 9 11 12 14 18))

(define (quick-sort<.v3 alon cmp)
  (cond
    [(empty? alon) '()]
    [(empty? (first alon)) (first alon)]
    [else (local ((define pivot (first alon))
                   (define tail (rest alon))) 
            (append (quick-sort<.v3 (filter (lambda (x) (cmp x pivot)) tail) cmp)
                    (list pivot)
                    (quick-sort<.v3 (filter (lambda (x) (not (cmp x pivot))) tail) cmp)))]))