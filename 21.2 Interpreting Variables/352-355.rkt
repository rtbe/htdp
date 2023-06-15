;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 352-355) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/abstraction)

; A BSL-var-expr (BSVE for short) is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

(define-struct add [left right])
(define-struct mul [left right])

;; BSL-expr (BSE for short) is one of:
; - Number
; - (make-add BSE BSE)
; - (make-mul BSE BSE)

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

;; Exercise 352

; BSVE Symbol Number -> BSVE
; consumes a BSL-var-expr ex, a Symbol x, and a Number v.
; It produces a BSL-var-expr like ex with all occurrences of x replaced by v

(check-expect (subst (make-add (make-mul 'x (make-mul 2 3))
          (make-mul 'x (make-mul -1 'x))) 'x 3.14)
              (make-add (make-mul 3.14 (make-mul 2 3))
          (make-mul 3.14 (make-mul -1 3.14))))

(define (subst ex x v)
  (match ex
    [(? number?) ex]
    [(? symbol?) (if (symbol=? ex x) v ex)]
    [(? add?) (make-add (subst (add-left ex) x v)
                        (subst (add-right ex) x v))]
    [(? mul?) (make-mul (subst (mul-left ex) x v)
                        (subst (mul-right ex) x v))]))

;; Exercise 353

; BSVE -> Boolean
; determines whether a BSL-var-expr is also a BSL-expr

(check-expect (numeric? 'x) #false)
(check-expect (numeric? 4) #true)
(check-expect (numeric? (make-add 4 5)) #true)
(check-expect (numeric? (make-add 4 'x)) #false)
(check-expect (numeric? (make-mul 4 8)) #true)
(check-expect (numeric? (make-mul 4 (make-add 3 'y))) #false)

(define (numeric? ex)
  (match ex
    [(? number?) #true]
    [(? add?) (and (numeric? (add-left ex))
                   (numeric? (add-right ex)))]
    [(? mul?) (and (numeric? (mul-left ex))
                   (numeric? (mul-right ex)))]
    [else #false]))

;; Exercise 354

; BSVE -> [Either Number Error]
; consumes a BSL-var-expr and determines its value if numeric? yields true for the input.
; Otherwise it signals an error

(check-expect (eval-variable 5) 5)
(check-error (eval-variable 'x))
(check-expect (eval-variable (make-add 2 (make-mul 2 3))) 8)
(check-error (eval-variable (make-add 2 (make-mul 2 'y))))

(define (eval-variable ex)
  (cond
        [(not (numeric? ex)) (error "ex is not numeric")]
        [(number? ex) ex]
        [(add? ex) (+ (eval-expression (add-left ex))
                      (eval-expression (add-right ex)))]
        [(mul? ex) (* (eval-expression (mul-left ex))
                      (eval-expression (mul-right ex)))]))

; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '()))

'((x 12) (y 15))

; BSVE AL -> [Either Number Error]
; consumes a BSL-var-expr ex and an association list da. Starting from ex, it iteratively applies subst to all associations in da. If numeric? holds for the result, it determines its value;
; otherwise it signals the same error as eval-variable

(define scope0 '((x 2) (y 3)))

; BSL-var-expr AL -> [Either Number Error]

(check-expect (eval-variable* (make-add (make-mul 'x 'y) 'y) scope0) 9)

(define (eval-variable* ex da)
  (eval-expression
   (foldl (lambda (a b) (subst b (first a) (second a))) ex da)))

;; Exercise 355

; BSL-var-expr AL -> [Either Number Error]

(check-expect (eval-var-lookup (make-mul 'x 'y) scope0) 6)

(define (eval-var-lookup ex da)
  (local (; Symbol AL -> Number

          ; replaces symbol with according value from da
          ; if there is no such a symbol in da returns an error
 
          (define (replace-sym ex da)
            (if (cons? (assq ex da))
                (second (assq ex da))
                (error "there is no such a symbol in AL"))))
  (match ex
    [(? number?) ex]
    [(? symbol?) (replace-sym ex da)]
    [(? add?) (+ (eval-var-lookup (add-left ex) da)
                 (eval-var-lookup (add-right ex) da))]
    [(? mul?) (* (eval-var-lookup (mul-left ex) da)
                 (eval-var-lookup (mul-right ex) da))])))


