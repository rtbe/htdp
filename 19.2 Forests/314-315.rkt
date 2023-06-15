;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 314-315) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/abstraction)

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

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

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


;; Exercise 314

; An FF (short for family forest) is [List-of FT]

; [List-of FT] -> Boolean
; does the forest contain any child with "blue" eyes
 
(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)
 
(define (blue-eyed-child-in-forest? a-forest)
  (ormap (lambda (tree) (blue-eyed-child? tree)) a-forest))

;; Exercise 315

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

(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)

; FT -> Number
; consumes a family tree and counts the child structures in the tree.
(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ 1 (+ (count-persons (child-father an-ftree)) (count-persons (child-mother an-ftree))))]))

; Number FT -> Number
;consumes a family tree and the current year.
; It produces the average age of all child structures in the family tree.

(define (average-age year an-ftree)
  (/ (sum-age year an-ftree)
     (count-persons an-ftree)))



; [List-of FT] -> Number
; it produces the average age of all child instances in the forest

(check-expect (average-age-in-forest 2000 ff2) (+ (/ 34 1) (/ (+ 35 74 74) 3)))


(define (average-age-in-forest year a-forest)
  (for/sum ([tree a-forest]) (average-age year tree)))

