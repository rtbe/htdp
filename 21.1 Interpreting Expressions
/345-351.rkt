;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 345-351) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/abstraction)

(define-struct add [left right])
(define-struct mul [left right])

;; Exercise 345

;; BSL-expr (BSE for short) is one of:
; - Number
; - (make-add BSE BSE)
; - (make-mul BSE BSE)

(make-add 10 -10)

(make-add (make-mul 20 3) 33)

(make-add (make-mul 3.14 (make-mul 2 3))
          (make-mul 3.14 (make-mul -1 -9)))

(+ -1 2)

(+ (* -2 -3) 33)

(* (+ 1 (* 2 3)) 3.14)

;; Exercise 346

; BSV is a data definition for the class of values to which
; a representation of a BSL expression can evaluate is one of:
; - Number

;; Exercise 347

; BSE -> Number
; consumes a representation of a BSL expression and computes its value

(check-expect (eval-expression (make-add (make-mul 3.14 (make-mul 2 3))
                                         (make-mul 3.14 (make-mul -1 -9))))
              (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9))))

(define (eval-expression bse)
  (cond
    [(number? bse) bse]
    [(add? bse) (+ (eval-expression (add-left bse))
                   (eval-expression (add-right bse)))]
    [(mul? bse) (* (eval-expression (mul-left bse))
                   (eval-expression (mul-right bse)))]))

;; Exercise 348

(define-struct and* [left right])
; A and* is a structure:
;   (make-and* Bool* Bool*)
; intepretation data representation of boolean AND expression

(define-struct or* [left right])
; A or* is a structure:
;   (make-or* Bool* Bool*)
; intepretation data representation of boolean OR expression

(define-struct not* [left])
; A not* is a structure:
;   (make-not* Bool*)
; intepretation data representation of boolean NOT expression

; Bool* id one of:
; - #true
; - #false
; - (make-and* Bool* Bool*)
; - (make-or* Bool* Bool*)
; - (make-not* Bool*)


; Bool* -> Boolean
; consumes (representations of) Boolean BSL expressions and computes their values

(check-expect (eval-bool-expression (make-and* #true (make-not* #false))) #true)

(define (eval-bool-expression b)
  (match b
    [(? boolean?) b]
    [(and* left right) (and (eval-bool-expression left)
                            (eval-bool-expression right))]
    [(or* left right) (or (eval-bool-expression left)
                          (eval-bool-expression right))]
    [(not* left) (not (eval-bool-expression left))]))
    
;; Exercise 349

; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
          
; An Atom is one of: 
; – Number
; – String
; – Symbol

(define WRONG "WRONG")

; Atom -> Boolean
(define (atom? a) (if (or (number? a) (string? a) (symbol? a)) #true #false))

; S-expr -> BSL-expr

(check-expect (parse 1) 1)
(check-error (parse "a") WRONG)
(check-error (parse 'a) WRONG)
(check-error (parse '(* 1 1 1)) WRONG)
(check-error (parse '(/ 1 1)) WRONG)
(check-expect (parse '(* 1 1)) (make-mul 1 1))
(check-expect (parse '(+ 1 1)) (make-add 1 1))

(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-expr 
(define (parse-sl s)
  (cond
    [(and (consists-of-3 s) (symbol? (first s)))
     (cond
       [(symbol=? (first s) '+)
        (make-add (parse (second s)) (parse (third s)))]
       [(symbol=? (first s) '*)
        (make-mul (parse (second s)) (parse (third s)))]
       [else (error WRONG)])]
    [else (error WRONG)]))



; Atom -> BSL-expr

(check-expect (parse-atom 1) 1)
(check-error (parse-atom "s") WRONG)
(check-error (parse-atom 's) WRONG)

(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))


; SL -> Boolean

(check-expect (consists-of-3 (list "a" "a" "a")) #true)
(check-expect (consists-of-3 (list "a")) #false)

(define (consists-of-3 s)
  (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
       (empty? (rest (rest (rest s))))))


;; Exercise 350

;; Exercise 351

; BSV is a data definition for the class of values to which
; a representation of a BSL expression can evaluate is one of:
; - Number
; - Error

; S-expr -> BSV
; accepts S-expressions. If parse recognizes them as BSL-expr, it produces their value

(check-expect (interpreter-expr '(* 1 2)) 2)
(check-expect (interpreter-expr '(+ 1 2)) 3)
(check-expect (interpreter-expr '(* 2 (+ 2 2))) 8)
(check-error (interpreter-expr "lol") WRONG)
(check-error (interpreter-expr '(/ 2 2)) WRONG)
(check-error (interpreter-expr '(+ 2 2 2)) WRONG)

(define (interpreter-expr sexp)
  (eval-expression (parse sexp)))
