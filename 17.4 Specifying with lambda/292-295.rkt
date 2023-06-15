;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 292-295) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 292

; [X X -> Boolean] [NEList-of X] -> Boolean 
; determines whether l is sorted according to cmp
 
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)
 
(define (sorted? cmp l)
  (cond
    [(empty? (rest l)) #true]
    [else (and (cmp (first l) (second l)) (sorted? cmp (rest l)))]))

; [X X -> Boolean] -> [[List-of X] -> Boolean]
; is the given list l0 sorted according to cmp
(define (sorted cmp)
  (lambda (l0)
      (if (empty? l0) #true (sorted? cmp l0))))

; sorted/l does not consumes cmp as an argument
; because it's already given to sorted function, so cmp will be in closure

;; Exercise 293

; X [Maybe List-of X] -> [[Maybe List-of X]  -> Boolean]

(define (found? x l0)
  (lambda (l1)
    (if (list? l1)
        (ormap (lambda (x) (member? x l1)) l1)
        ; why
        (ormap (lambda (i) (not (equal? i x))) l0))))


; [X] X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))

(check-satisfied (find 2 '(1 2 3)) (found? 2 '(1 2 3)))
(check-satisfied (find 2 '(1 4 3)) (found? 2 '(1 4 3)))


;; Exercise 294

; [X] [List-of-X] -> [[Maybe N]] -> Boolean]

(define (index? x l)
  (lambda (i)
    (if (boolean? i)
        (ormap (lambda (n) (not (equal? i x))) l)
        (ormap (lambda (n) (member? i l)) l))))



; [X] X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

(define a-list '(1 2 3 4 -1 500))

(check-satisfied (index 2 a-list) (index? 2 a-list))
(check-satisfied (index 5 a-list) (index? 5 a-list))

