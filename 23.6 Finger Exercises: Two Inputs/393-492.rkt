;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 393-492) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)
(require racket/list)

;; Exercise 393

; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)
; 
; Son is used when it 
; applies to Son.L and Son.R
  

; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s

; Son Son -> Son
;  consumes two sets and produces one that contains the elements of both

(check-expect  (union '() (list 4 5)) (list 4 5))
(check-expect  (union (list 1 2 3) '()) (list 1 2 3))
(check-expect  (union (list 1 2 3) (list 4 5)) (list 4 5 1 2 3))
(check-expect  (union (list 1 2 3) (list 4 5 6)) (list 4 5 6 1 2 3))
(check-expect  (union (list 1 2) (list 4 5 6)) (list 4 5 6 1 2))


(define (union s1 s2)
  (foldr (lambda (x y) (if (member? x y) y (cons x y))) s1 s2))

; Son Son -> Son
; consumes two sets and produces the set of exactly those elements that occur in both

(check-expect  (intersect '() (list 4 5)) '())
(check-expect  (intersect (list 1 2 3) '()) '())
(check-expect  (intersect (list 1 2 3) (list 4 5)) '())
(check-expect  (intersect (list 1 2 3) (list 1 2 3)) (list 1 2 3))
(check-expect  (intersect (list 1 2 3) (list 2 3)) (list 2 3))
(check-expect  (intersect (list 2 3) (list 1 2 3)) (list 2 3))


(define (intersect s1 s2)
  (filter (lambda (x) (member? x s2)) s1))

;; Exercise 394

; [List-of Number] [List-of Number] -> [List-of Number]
; consumes two lists of numbers, sorted in ascending order.
; It produces a single sorted list of numbers that contains all the numbers on both inputs lists.
; A number occurs in the output as many times as it occurs on the two input lists together

(check-expect (merge (list 1 2 3) '()) (list 1 2 3))
(check-expect (merge '() (list 1 2 3)) (list 1 2 3))
(check-expect (merge (list 1 2 3) (list 1 2 3)) (list 1 1 2 2 3 3))
(check-expect (merge (list 1 2 3) (list 1 2)) (list 1 1 2 2 3))
(check-expect (merge (list 1 2) (list 1 2 3)) (list 1 1 2 2 3))
(check-expect (merge (list 1 2 3) (list 1 2)) (list 1 1 2 2 3))
(check-expect (merge (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))

(define (merge lon1 lon2)
  (cond
    [(and (empty? lon1) (empty? lon2)) '()]
    [(and (empty? lon1) (not (empty? lon2))) lon2]
    [(and (empty? lon2) (not (empty? lon1))) lon1]
    [else (cond
            [(= (first lon1) (first lon2))
             (cons (first lon1)
                   (cons (first lon2)
                         (merge (rest lon1) (rest lon2))))]
            [(< (first lon1) (first lon2))
             (cons (first lon1) (merge (rest lon1) lon2))]
            [else (cons (first lon2) (merge lon1 (rest lon2)))])]))

;; Exercise 395

; [List-of X] Number -> [List-of X]
; consumes a list l and a natural number n.
; It produces the first n items from l or all of l if it is too short

(check-expect (take.v2 (list 1 2 3) 3) (list 1 2 3))
(check-expect (take.v2 (list 1 2 3) 5) (list 1 2 3))
(check-expect (take.v2 (list 1 2 3) 2) (list 1 2))
(check-expect (take.v2 (list 1) 1) (list 1))
(check-expect (take.v2 (list 1) 0) '())

(define (take.v2 l n)
  (cond
    [(empty? l) '()]
    [(= n 0) '()]
    [else (cons (first l) (take.v2 (rest l) (- n 1)))]))


; [List-of X] Number -> [List-of Number]
; consumes a list l and a natural number n.
; Its result is l with the first n items removed or just ’() if l is too short.

(check-expect (drop.v2 (list 1 2 3) 3) '())
(check-expect (drop.v2 (list 1 2 3) 5) '())
(check-expect (drop.v2 (list 1 2 3) 2) (list 3))
(check-expect (drop.v2 (list 1) 1) '())
(check-expect (drop.v2 (list 1) 0) (list 1))

(define (drop.v2 l n)
  (cond
    [(empty? l) '()]
    [(= n 0) l]
    [else (drop.v2 (rest l) (- n 1))]))

;; Exercise 396

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed


(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))
 
; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))


 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; [List-of String] HM-Word KeyEvent -> HM-Word
(check-expect (compare-word (explode "hola")
                            (explode "____")
                            "o")
  (explode "_o__"))
(check-expect (compare-word (explode "hola")
                            (explode "ho_a")
                            "z")
              (explode "ho_a"))

(define (compare-word word status ke)
  (cond
    [(not (member? ke word)) status]
    [else  (if (and (string=? (first word) ke)
                    (string=? (first status) "_"))
               (cons ke (compare-word (rest word) (rest status) ke))
               (cons (first status) (compare-word (rest word) (rest status) ke)))]))

;; Exercise 397

(define-struct time-card [id hours])
; - (make-time-card Number Number)

(define-struct employee-record [name id pay-rate])
; - (make-employee-record String Number Number)

(define-struct wage-record [name wage])
; - (make-employee-record String Number)

; [List-of Employee-record] [List-of Time-card] -> [List-of Wage-record]
; consumes a list of employee records and a list of time-card records.
; It produces a list of wage records, which contain the name and weekly wage of an employee.
; The function signals an error if it cannot find an employee record for a time card or vice versa

(check-expect (wages*.v3 (list (make-employee-record "Bob" 1 1)
                               (make-employee-record "Rob" 2 2)
                               (make-employee-record "Zak" 3 3))
                         (list (make-time-card 3 30)
                               (make-time-card 1 10)
                               (make-time-card 2 20)))
               (list (make-wage-record "Bob" 10)
                     (make-wage-record "Rob" 40)
                     (make-wage-record "Zak" 90)))

(check-error (wages*.v3 (list (make-employee-record "Bob" 1 1)
                              (make-employee-record "Zak" 3 3))
                        (list (make-time-card 3 30)
                              (make-time-card 1 10)
                              (make-time-card 2 20))))

(check-error (wages*.v3 (list (make-employee-record "Bob" 1 1)
                              (make-employee-record "Rob" 2 2)
                              (make-employee-record "Zak" 3 3))
                        (list (make-time-card 3 30)
                              (make-time-card 2 20))))

(define (wages*.v3 loer lotc)
  (local ((define sorted-loer (sort loer (lambda (x y) (if (< (employee-record-id x)
                                                              (employee-record-id y))
                                                           #true
                                                           #false))))
          (define sorted-lotc (sort lotc (lambda (x y) (if (< (time-card-id x)
                                                              (time-card-id y))
                                                           #true
                                                           #false))))
          
          ; [List-of Employee-record] [List-of Time-card] -> [List-of Wage-record]
          (define (work-records loer lotc)
            (cond
              [(and (empty? loer) (empty? lotc)) '()]
              [(and (empty? loer) (not (empty? lotc))) (error "loer is empty but lotc is not")]
              [(and (not (empty? loer)) (empty? lotc)) (error "loer is not empty but lotc is empty")]
              [else (cons (make-wage-record (employee-record-name (first loer))
                                            (* (employee-record-pay-rate (first loer))
                                               (time-card-hours (first lotc))))
                  (work-records (rest loer) (rest lotc)))])))
          (work-records sorted-loer sorted-lotc)))

;; Exercise 398

; [List-of Number] [List-of Number] -> Number
; Calculates the value of a linear combination for a given input
(check-expect (value '() '()) 0)
(check-expect (value '(0) '(5)) 0)
(check-expect (value '(1) '(5)) 5)
(check-expect (value '(2 3) '(5 10)) 40)

(define (value lc vals)
  (cond
   [(and (empty? lc) (empty? vals)) 0]
   [else (+ (* (first lc) (first vals))
            (value (rest lc) (rest vals)))]))

;; Exercise 399

; [List-of String] -> [List-of String] 
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick
    (non-same names (arrangements names))))
 
; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
(define arrangements permutations)

; [NEList-of X] -> X 
; returns a random item from the list 
(define (random-pick l)
  (list-ref l (random (length l))))
 
; [List-of String] [List-of [List-of String]] -> [List-of [List-of String]]
; produces the list of those lists in ll that do not agree with names at any place 

(check-expect (non-same '("a" "b" "c")
                        '(("a" "b" "c") ("b" "a" "c") ("c" "a" "b"))) '(("c" "a" "b")))

(define (non-same names ll)
  (local (
          (define (agree? names l)
            (cond
              [(empty? names) #false]
              [else (or (if (string=? (first names) (first l)) #true #false)
                        (agree? (rest names) (rest l)))])))
    (filter (lambda (x) (not (agree? names x))) ll)))


;; Exercise 400

; [List-of Symbol] [List-of Symbol] -> Boolean
; function returns #true if the pattern is identical to the initial part of the search string
; otherwise it returns #false.

(check-expect (DNAPrefix '(a c g) '(a c g g g)) #true)
(check-expect (DNAPrefix '() '(a c g g g)) #true)
(check-expect (DNAPrefix '(a c) '(g g g)) #false)
(check-expect (DNAPrefix '(a c) '(a)) #false)

(define (DNAPrefix pattern search-str)
  (cond
    [(empty? pattern) #true]
    [(and (not (empty? pattern)) (empty? search-str)) #false]
    [else (and (if (symbol=? (first pattern) (first search-str))
                   #t
                   #f)
                   (DNAPrefix (rest pattern) (rest search-str)))]))

; [List-of Symbol] [List-of Symbol] -> [List-of Symbol] or Error or Boolean
(check-expect (DNAdelta '(a c g) '(a c g g g)) '(g g))
(check-expect (DNAdelta '() '(a c g)) '(a c g))
(check-expect (DNAdelta '(a c) '(g g g)) #false)
(check-expect (DNAdelta '(a c) '(a)) #false)
(check-error (DNAdelta '(a c) '(a c)))

(define (DNAdelta pattern search-str)
  (cond
    [(and (empty? pattern) (empty? search-str)) (error "there is no letter beyond the given pattern")]
    [(and (empty? pattern) (not (empty? search-str))) search-str]
    [(and (not (empty? pattern)) (empty? search-str)) #f]
    [else (if (symbol=? (first pattern) (first search-str))
                   (DNAdelta (rest pattern) (rest search-str))
                   #f)]))

;; Exercise 401

; An S-expr (S-expression) is one of: 
; – Atom
; – [List-of S-expr]
; 
; An Atom is one of: 
; – Number
; – String
; – Symbol

; S-expr S-expr -> Boolean
(check-expect (sexp=? 'a 'a) #t)
(check-expect (sexp=? 'a 'b) #f)
(check-expect (sexp=? '(a) '(a)) #t)
(check-expect (sexp=? '(a b) '(a b)) #t)
(check-expect (sexp=? '(a b c) '(a b c)) #t)
(check-expect (sexp=? '(a b c) '(a b d)) #f)
(check-expect (sexp=? '(a b (c)) '(a b (c))) #t)
(check-expect (sexp=? '(a b (c)) '(a b (d))) #f)

(define (sexp=? sexp1 sexp2)
  (local (
          (define (atom? sexp) (or (number? sexp)
                                   (string? sexp)
                                   (symbol? sexp)))
          (define (compare-atoms a1 a2)
            (cond
              [(and (number? sexp1)
                    (number? sexp2)) (= sexp1 sexp2)]
              [(and (string? sexp1)
                    (string? sexp2)) (string=? sexp1 sexp2)]
              [(and (symbol? sexp1)
                    (symbol? sexp2)) (symbol=? sexp1 sexp2)])))
    (cond
      [(and (atom? sexp1) (atom? sexp2)) (compare-atoms sexp1 sexp2)]
      [(and (empty? sexp1)
          (empty? sexp2)) #t]
      [(and (cons? sexp1)
          (cons? sexp2)) (and (sexp=? (first sexp1) (first sexp2))
                              (sexp=? (rest sexp1) (rest sexp2)))]
      [else #f])))
