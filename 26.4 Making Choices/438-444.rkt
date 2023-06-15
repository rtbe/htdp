;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 438-444) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))

(define (gcd-generative n m)
  (local (; N[>= 1] N[>=1] -> N
          ; generative recursion
          ; (gcd L S) == (gcd S (remainder L S)) 
          (define (clever-gcd L S)
            (cond
              [(= S 0) L]
              [else (clever-gcd S (remainder L S))])))
    (clever-gcd (max m n) (min m n))))

;; Exercise 438

; It iterates over (min n m) with substraction of 1 on every iteration
; to find out greatest common divisor

; It testsj for every number between the smaller of n and m and 1 whether it divides
; both n and m evenly and returns the first such number

;; It uses min of n and m since it can be greatest common divisor,
; but  max value is guaranteed not to be greatest divisor.
; For example (6 3) - we can not divide 3 by 6 without remainder,
; but we can divide 6 by 3 without remainder

;; Exercise 439

; (time (gcd-structural 101135853 45014640))

;; Exercise 440

(time (gcd-generative 101135853 45014640))

;; Exercise 441

; amounts of append and applications of quick-sort< are the same in both cases

;; Exercise 442

; [List-of Number] [[List-of Number] -> [List-of Number]] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct

(check-expect (quick-sort< '(11 9 2 18 12 14 4 1))
                           '(1 2 4 9 11 12 14 18))

(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty? (first alon)) (first alon)]
    [else (local ((define pivot (first alon))
                   (define tail (rest alon))) 
            (append (quick-sort< (filter (lambda (x) (< x pivot)) tail))
                    (list pivot)
                    (quick-sort< (filter (lambda (x) (not (< x pivot))) tail))))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort< l)
  (cond
    [(empty? l) '()]
    [else  (insert (first l) (sort< (rest l)) )]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else 
      (if (< n (first l))
          (cons n l)
          (cons (first l) (insert n (rest l))))]))


(define (create-tests n)
  (cond
    [(= n 0) '()]
    [else (cons (random 100) (create-tests (sub1 n)))]))

(create-tests 5)

(< (time (quick-sort< (create-tests 150))) (time (sort< (create-tests 150))))

(define (clever-sort alon)
  (cond
    [(< (length alon) 100) (sort< alon)]
    [else (quick-sort< alon)]))

;; Exercise 443

; Why is it impossible to find a divisor with this strategy?
; Because each of sub-problems are not composing into a given one to give final result.
; Each of sub-problems is just diffirent problem for diffirent numbers


;; Exercise 444

; Why do you think divisors consumes two numbers?

; Why does it consume S as the first argument in both uses?
; Because we can not divide L by S without any remainder 