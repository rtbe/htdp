;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 311-313) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct no-parent [])
(define NP (make-no-parent))

(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> Boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field
 
(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
 
(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))


;; Exercise 311

(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)

; FT -> Number
; consumes a family tree and counts the child structures in the tree.
(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ 1 (+ (count-persons (child-father an-ftree)) (count-persons (child-mother an-ftree))))]))


; Exercise 311


; Number FT -> Number
; consumes a family tree and the current year.
; It produces the sum of age of all child structures in the family tree

(check-expect (sum-age 2000 Carl) 74)
(check-expect (sum-age 2000 Gustav) (+ 74 74 34 35 12))

(define (sum-age year an-ftree)
   (cond
    [(no-parent? an-ftree) 0]
    [else (+ (- year (child-date an-ftree))
             (sum-age year (child-father an-ftree))
             (sum-age year (child-mother an-ftree)))]))

; Number FT -> Number
;consumes a family tree and the current year.
; It produces the average age of all child structures in the family tree.

(check-expect (average-age 2000 Carl) (/ 74 1))
(check-expect (average-age 2000 Gustav) (/ (+ 74 74 34 35 12) 5))

(define (average-age year an-ftree)
  (/ (sum-age year an-ftree)
     (count-persons an-ftree)))

;; Exercise 312

; FT -> [List-of String]
; consumes a family tree and produces a list of all eye colors in the tree

(check-expect (eye-colors Carl) '("green"))
(check-expect (eye-colors Dave) '("black" "green" "green"))

(define (eye-colors an-ftree)
     (cond
       [(no-parent? an-ftree) '()]
       [else (cons (child-eyes an-ftree)
                   (append
                    (eye-colors (child-father an-ftree))
                    (eye-colors (child-mother an-ftree))))]))

;; Exercise 313

; FT -> Boolean
; which is like blue-eyed-child? but responds with #true only when a proper ancestor,
; not the given child itself, has blue eyes

(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)

(define (blue-eyed-ancestor? an-ftree)
     (cond
       [(no-parent? an-ftree) #false]
       [else (or
              (if (not (no-parent? (child-father an-ftree)))
                  (string=? "blue" (child-eyes (child-father an-ftree)))
                  #false)
              (if (not (no-parent? (child-mother an-ftree)))
                  (string=? "blue" (child-eyes (child-mother an-ftree)))
                  #false)
              (blue-eyed-ancestor? (child-father an-ftree))
              (blue-eyed-ancestor? (child-mother an-ftree)))]))

; or you can reuse blue-eyed-child? on (child-father an-ftree) and (child-mother an-ftree)

