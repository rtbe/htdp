;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 166-170) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work String Number Number)
; interpretation (make-work n r h) combines the name 
; with the pay rate r and the number of hours h

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees

'()
(cons (make-work "Robby" 11.95 39)
      '())
(cons (make-work "Matthew" 12.95 45)
      (cons (make-work "Robby" 11.95 39)
            '()))

; Low -> List-of-numbers
; computes the weekly wages for all weekly work records 
 
(check-expect
  (wage*.v2 (cons (make-work "Robby" 11.95 39) '()))
  (cons (* 11.95 39) '()))
 
(define (wage*.v2 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low) (cons (wage.v2 (first an-low))
                          (wage*.v2 (rest an-low)))]))
 
; Work -> Number
; computes the wage for the given work record w
(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))

;; Exercie 166

(define-struct paycheck [employee wage])
; A (piece of) Paycheck is a structure:
; (make-paycheck String Number)
; interpretation (make-paycheck n w) combines the name n
; with the wage w

; Lop (short for list of paychecks) is one of: 
; – '()
; – (cons Paycheck Lop)
; interpretation an instance of Lop represents the 
; name of emploee and it's wage

; Low -> Lop
; computes the paychecks for all weekly work records
(check-expect
  (wage*.v3 (cons (make-work "Robby" 11.95 39) '()))
  (cons (make-paycheck "Robby" (* 11.95 39)) '()))

(define (wage*.v3 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low) (cons (make-paycheck (work-employee (first an-low)) (wage.v2 (first an-low))) (wage*.v3 (rest an-low)))]))


(define-struct employee [number name])
; A Employee is a structure:
; (make-emploee Number String)
; interpretation (make-employee number name) combines the number
; with the name of employee

(define-struct work.v2 [employee rate hours])
; A (piece of) Work.v2 is a structure: 
;   (make-work.v2 Employee Number Number)
; interpretation (make-work.v2 n r h) combines the Employee 
; with the pay rate r and the number of hours h

; Work.v2 -> Number
; computes the wage for the given work record w
(define (wage.v3 w)
  (* (work.v2-rate w) (work.v2-hours w)))

; Low.v2 (short for list of works) is one of: 
; – '()
; – (cons Work.v2 Low.v2)
; interpretation an instance of Low.v2 represents the 
; hours worked for a number of employees

(define-struct paycheck.v2 [employee wage])
; A (piece of) Paycheck is a structure:
; (make-paycheck.v2 Employee Number)
; interpretation (make-paycheck n w) combines the EmployeeN
; with the wage w

; Lop.v2 (short for list of paychecks) is one of: 
; – '()
; – (cons Paycheck.v2 Lop)
; interpretation an instance of Lop.v2 represents the emploee and it's wage

; Low.2 -> Lop.v2
; computes the paychecks.v2 for all weekly work records
(check-expect
  (wage*.v4 (cons (make-work.v2 (make-employee 123 "Robby") 11.95 39) '()))
  (cons (make-paycheck.v2 (make-employee 123 "Robby") (* 11.95 39)) '()))

(define (wage*.v4 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low)
     (cons
      (make-paycheck.v2
       (make-employee
        (employee-number (work.v2-employee (first an-low)))
        (employee-name (work.v2-employee (first an-low))))
       (wage.v3 (first an-low)))
      (wage*.v4 (rest an-low)))]))

;; Exercise 167

; A List-of-posns is one of:
; - '()
; - (cons Posn List-of-posns)
; interpretation is a list of Posn

; List-of-posns -> Number
; produces the sum of all its x-coordinates
(check-expect (sum (cons (make-posn 1 2) (cons (make-posn 3 4) '()))) 4)

(define (sum lop)
  (cond
   [(empty? lop) 0]
   [else (+ (posn-x (first lop)) (sum (rest lop)))]))

;; Exercise 168

; List-of-posns -> List-of-posns
; takes List-of-posns and produces List-of-posns for each (make-posn x y) in the former,
; the latter contains (make-posn x (+ y 1))
(check-expect (translate (cons (make-posn 1 2) (cons (make-posn 3 4) '()))) (cons (make-posn 1 3) (cons (make-posn 3 5) '())))

(define (translate lop)
  (cond
   [(empty? lop) '()]
   [else (cons (make-posn (posn-x (first lop)) (+ (posn-y (first lop)) 1))  (translate (rest lop)))]))
                               
                               
;; Exercise 169

; List-of-posns -> List-of-posns
; produces List-of-posn whose x x-coordinates are between 0 and 100
; and whose y-coordinates are between 0 and 200
(check-expect (legal (cons (make-posn 1 2) (cons (make-posn 3 4) '()))) (cons (make-posn 1 2) (cons (make-posn 3 4) '())))
(check-expect (legal (cons (make-posn 101 2) (cons (make-posn 3 4) '()))) (cons (make-posn 3 4) '()))
(check-expect (legal (cons (make-posn 10 201) (cons (make-posn 3 4) '()))) (cons (make-posn 3 4) '()))
(check-expect (legal (cons (make-posn 101 2) (cons (make-posn 300 4) '()))) '())

(define (legal lop)
  (cond
    [(empty? lop) '()]
    [else
     (if
      (and (<= (posn-x (first lop)) 100) (<= (posn-y (first lop)) 200))
      (cons (first lop) (legal (rest lop)))
      (legal (rest lop)))]))

;; Exercise 170

(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999.

; Loph (short for list of phones) is one of: 
; – '()
; – (cons Phone Loph)
; interpretation an instance of Loph represents the 
; phone number with area switch and four

; Loph -> Loph
; produces List-of-phones, with replaced area code 713 with 281
(check-expect (replace
               (cons (make-phone 1 2 3) (cons (make-phone 713 2 3) '())))
               (cons (make-phone 1 2 3) (cons (make-phone 281 2 3) '())))

(define (replace loph)
  (cond
    [(empty? loph) '()]
    [else (cons
           (make-phone
            (if (= (phone-area (first loph)) 713) 281 (phone-area (first loph)))
            (phone-switch (first loph))
            (phone-four (first loph)))
           (replace(rest loph) ))]))





    