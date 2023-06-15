;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 300-309) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 300

(define (p1 x y)
  (+ (* x y)
     (+ (* 2 x)
        (+ (* 2 y) 22))))
 
(define (p2 x)
  (+ (* 55 x) (+ x 11)))
 
(define (p3 x)
  (+ (p1 x 0)
     (+ (p1 x 1) (p2 x))))

;; Exercise 301

(define (insertion-sort alon)
  (local ((define (sort alon)
            (cond
              [(empty? alon) '()]
              [else
               (add (first alon) (sort (rest alon)))]))
          (define (add an alon)
            (cond
              [(empty? alon) (list an)]
              [else
               (cond
                 [(> an (first alon)) (cons an alon)]
                 [else (cons (first alon)
                             (add an (rest alon)))])])))
    (sort alon)))

(define (sort* alon)
  (local ((define (sort alon)
            (cond
              [(empty? alon) '()]
              [else
               (add (first alon) (sort (rest alon)))]))
          (define (add an alon)
            (cond
              [(empty? alon) (list an)]
              [else
                (cond
                  [(> an (first alon)) (cons an alon)]
                  [else (cons (first alon)
                              (add an (rest alon)))])])))
    (sort* alon)))

;; No, they are same

;; Exercise 302

;; x is used here before its definition
; (define x (cons 1 x))

;; Exercise 303

(lambda (x y)
  (+ x (* x y)))
 
(lambda (x y)
  (+ x
     (local ((define x (* y y)))
       (+ (* 3 x)
          (/ 1 x)))))
 
(lambda (x y)
  (+ x
     ((lambda (x)
        (+ (* 3 x)
           (/ 1 x)))
      (* y y))))

;; Exercise 304

(require 2htdp/abstraction)

(check-expect (for/list ([i 2] [j '(a b)]) (list i j))
              (list (list 0 'a) (list 1 'b)))

(check-expect (for*/list ([i 2] [j '(a b)]) (list i j))
              (list (list 0 'a) (list 0 'b) (list 1 'a) (list 1 'b)))


;; Exercise 305

(define RATE 1.06)

;; converts a list of us dollars into a list of euros

(check-expect (convert-euro (list 1 2 3 4 5))
              (list (* 1 1.06) (* 2 1.06) (* 3 1.06) (* 4 1.06) (* 5 1.06)))

(define (convert-euro lon) (for/list ([i lon]) (* i RATE)))

;; Exercise 306

; creates the list (list 0 ... (- n 1)) for any natural number n
(check-expect (create-list* 3) (list 0 1 2))

(define (create-list* n) (for/list ([i n]) i))

; creates the list (list 1 ... n) for any natural number n
(check-expect (create-list** 3) (list 0 1 2 3))

(define (create-list** n) (for/list ([i (+ 1 n)]) i))

; creates the list (list 1 1/2 ... 1/n) for any natural number n;
(check-expect (create-list*** 3) (list 0 1 (/ 1 2) (/ 1 3)))

(define (create-list*** n) (for/list ([i (+ 1 n)]) (if (zero? i) 0 (/ 1 i))))

; creates the list of the first n even numbers
(check-expect (create-list**** 5) (list 2 4 6 8 10))
(check-expect (create-list**** 10) (list 2 4 6 8 10 12 14 16 18 20))

(define (create-list**** n) (for/list ([i (in-range 2 (* (+ n 1) 2) 2)]) i))

; creates a diagonal square of 0s and 1s; see exercise 262.
(check-expect (identityM 3)
(list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

(define (identityM n)
  (local (; Number Number -> [List-of Number]
          ; returns a list of num elements with 1 at pos
          (define (make-row num pos)
            (cond
              [(= num n) '()]
              [else (cons (if (= num pos) 1 0)
                          (make-row (+ num 1) pos))])))
    (for/list ([i n]) (make-row 0 i))))


;; Exercise 307

; String [List-of String] -> [Maybe String]
; retrieves the first name on the latter that is equal to,
; or an extension of, the former.

(check-expect (find-name "Vas" (list "Opr" "Orh" "Zsa" "Vasya")) "Vasya")

(define (find-name n lon) (for/or ([i lon]) (if (string-contains? n i) i #false)))

; Number [List-of String] -> Boolean
; ensures that no name on some list of names exceeds some given width

(check-expect (name-smaller-than 10 (list "Opr" "Orh" "Zsa" "Vasya")) #true)
(check-expect (name-smaller-than 2 (list "Opr" "Orh" "Zsa" "Vasya")) #false)

(define (name-smaller-than n lon) (for/and ([i lon]) (< (string-length i) n)))

;; Exercise 308


;; Exercise 309

(define-struct phone [area switch four])

; [List-of Phone] -> [List-of Phone]
; substitutes the area code 713 with 281 in a list of phone records

(check-expect (replace (list (make-phone 713 11 1) (make-phone 281 11 1)))
              (list (make-phone 281 11 1) (make-phone 281 11 1)))

(define (replace lop)
    (match lop
      ['() '()]
      [(cons (phone 713 switch four) tail)
       (cons (make-phone 281 switch four) (replace tail))]
      [(cons head tail) (cons head tail)]))

;; Exercise 309

; [List-of [List-of String]] -> [List-of Nuber]
; determines the number of Strings per item in a list of list of strings

(check-expect (words-on-line (list (list "a" "v") (list "z") (list "sda" "asd")))
              (list 2 1 2))

(define (words-on-line lls)
  (match lls
    ['() '()]
    [(cons head tail) (cons (length head) (words-on-line tail))]))
