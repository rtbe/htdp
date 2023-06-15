;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 445-451) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define EPSILON 0.0001)

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption

(define (find-root f left right)
  (cond
    [(<= (- right left) EPSILON) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
           (find-root f left mid)]
          [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
           (find-root f mid right)]))]))

;; Exercise 445

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; [Number -> Boolean]
(define (poly? x) (<= (abs (poly x)) 0.5))

(poly? (find-root poly 1 3))

;; Exercise 446

(check-satisfied (find-root poly 1 3) poly?)
(check-satisfied (find-root poly 4 6) poly?)

;; Exercuse 447

(find-root poly -1 5)

;; Exercise 448

; Q: The find-root algorithm terminates for all (continuous) f, left,
; and right for which the assumption holds.
; Why? Formulate a termination argument.
; A: Function divides search space by half on every iteration, so sooner or later function will find root.
; But this is possible only if f is continous

; Q: Suppose the arguments of find-root describe an interval of size S1. How large is the distance between left and right for the first and second recursive call to find-root?
; After how many steps is (- right left) smaller than or equal to ε?
; A: In general it will take O(log n) steps

;; Exercise 449


(check-satisfied (find-root.v2 poly 1 3) poly?)
(check-satisfied (find-root.v2 poly 4 6) poly?)

(define (find-root.v2 f left right)
  (local ((define (find-root.v2* f left right f@left f@right)
            (cond
              [(<= (- right left) EPSILON) left]
              [else
               (local ((define mid (/ (+ left right) 2))
                       (define f@mid (f mid)))
                 (cond
                   [(or (<= f@left 0 f@mid) (<= f@mid 0 f@left))
                    (find-root.v2* f left mid f@left f@right)]
                   [(or (<= f@mid 0 f@right) (<= f@right 0 f@mid))
                    (find-root.v2* f mid right f@left f@right)]))])))
    (find-root.v2* f left right (f left) (f right))))

;; Exercise 450

(define (find-root.v2** f left right)
  (local ((define (find-root.v2*** f left right f@left f@right)
            (cond
              [(<= (- right left) EPSILON) left]
              [else
               (local ((define mid (/ (+ left right) 2))
                       (define f@mid (f mid)))
               (if (positive? f@mid)
                   (find-root.v2*** f left mid f@left f@right)
                   (find-root.v2*** f mid right f@left f@right)))])))
    (find-root.v2*** f left right (f left) (f right))))

(find-root.v2** (lambda (x) (+ x 1)) -10 10)


;; Exercise 451

(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

(define table-incr (make-table 10 (lambda (i) (- (+ i 1) 10))))

; Table -> Number
; consumes a monotonically increasing table
; and finds the smallest index for a root of the table

(check-expect (find-linear table-incr) 9)

(define (find-linear t)
  (local ((define len (table-length t))
          
          (define (find-index i)
            (cond
              [(= i len) (error "there is no root in a given table")]
              [else (if (= ((table-array t) i) 0)
                        i
                        (find-index (add1 i)))])))
    (find-index 0)))

; Table -> Number
; consumes a monotonically increasing table
; and finds the smallest index for a root of the table

(check-expect (find-binary table-incr) 9)

(define (find-binary t)
  (local ((define len (table-length t))
          (define array (table-array t))
          (define (find-index left right)
            (local ((define mid-idx (floor (/ (+ left right) 2)))
                    (define mid-val (array mid-idx)))
              (cond
                [(= (array left) 0) left]
                [(= (array right) 0) right]
                [(= mid-val 0) mid-idx]
                [else (if (positive? mid-val)
                          (find-index left mid-idx)
                          (find-index mid-idx right))]))))
    (find-index 0 len)))