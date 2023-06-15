;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 356-359) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/abstraction)

(define-struct add [left right])
(define-struct mul [left right])


;; Exercise 356

; A BSL-fun-expr (BSLFE for short) is one of: 
; – Number
; – Symbol
; – (make-add BSL-fun-expr BSL-fun-expr)
; – (make-mul BSL-fun-expr BSL-fun-expr)
; – (make-fun Symbol BSL-fun-expr)

(define-struct fun [name arg])
; - (make-fun Symbol BSLFE
; fun is a representation of a function with name and one arg

(define exp1 (make-fun 'k (make-add 1 1))) ; (k (+ 1 1))

(define exp2 (make-mul 5 (make-fun 'k (make-add 1 1)))) ; (* 5 (k (+ 1 1)))

(define exp3 (make-mul (make-fun 'i 5) (make-fun 'k (make-add 1 1)))) ; (* (i 5) (k (+ 1 1)))

;; Exercise 357

(define add-5-name 'add-5)
(define add-5-arg-name 'x)
(define add-5-body (make-add 5 add-5-arg-name))

; BSLFE Symbol Symbol BSLFE ->
; Evaluates `ex` assuming there is only the function 
; `fn-name` in the scope, with the argument `arg`,
; and its body is the expression `body`

(check-expect (eval-definition1 (make-add 1 1) add-5-name add-5-arg-name add-5-body) 2)

(check-expect (eval-definition1 (make-add (make-fun 'add-5 3) 1) add-5-name add-5-arg-name add-5-body) 8)

(define (eval-definition1 ex fname argname b)
  (match ex
    [(? number?) ex]
    [(? symbol?) (error "there should be no variables")]
    [(add l r) (+ (eval-definition1 l fname argname b)
                  (eval-definition1 r fname argname b))]
    [(mul l r) (* (eval-definition1 l fname argname b)
                  (eval-definition1 r fname argname b))]
    [(fun name expr) (if (symbol=? name fname)
                         (eval-definition1
                          (subst expr argname (eval-definition1 expr fname argname b))
                          fname
                          argname
                          b)
                         (error "there is no such a function"))]))

; BSLFE Symbol Number -> BSLFE
; consumes a BSLFE ex, a Symbol x, and a Number v.
; It produces a BSLFE like ex with all occurrences of x replaced by v
(define (subst ex x v)
  (match ex
    [(? number?) ex]
    [(? symbol?) (if (symbol=? ex x) v ex)]
    [(add l r) (make-add (subst l x v)
                         (subst r x v))]
    [(mul l r) (make-mul (subst l x v)
                         (subst r x v))]
    [(fun name expr) (make-fun name (subst expr x v))]))