;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 388-389) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; [List-of Number] [List-of Number] -> [List-of Number]
; multiplies the corresponding items on 
; hours and wages/h 
; assume the two lists are of equal length

(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list 5.65) (list 40))
              (list 226.0))
(check-expect (wages*.v2 '(5.65 8.75) '(40.0 30.0))
              '(226.0 262.5))

(define (wages*.v2 hours wages/h)
  (cond
    [(empty? hours) '()]
    [else
     (cons
       (weekly-wage (first hours) (first wages/h))
       (wages*.v2 (rest hours) (rest wages/h)))]))

; Number Number -> Number
; computes the weekly wage from pay-rate and hours
(define (weekly-wage pay-rate hours)
  (* pay-rate hours))

;; Exercise 388

(define-struct employee [name ssn pay-rate])
; An Employee is a structure
; - (make-employee String Number Number)

(define-struct work-record [name hours])
; A Work-record is a structure:
; - (make-work-record String Number)

; [List-of Employee] [List-of Work-record] -> [List-of Number]
; multiplies the corresponding items on 
; hours and wages/h 
; assume the two lists are of equal length

(check-expect (wages*.v3 '() '()) '())
(check-expect (wages*.v3 (list (make-employee "Sam" 123 5.65)) (list (make-work-record "Sam" 40)))
              (list 226.0))
(check-expect (wages*.v3 (list (make-employee "Sam" 123 5.65)
                           (make-employee "Bob" 321 8.75))
                         (list (make-work-record "Sam" 40) (make-work-record "Bob" 30.0)))
              '(226.0 262.5))

(define (wages*.v3 le lwr)
  (cond
    [(empty? le) '()]
    [else
     (cons
       (weekly-wage.v2 (employee-pay-rate (first le)) (work-record-hours (first lwr)))
       (wages*.v3 (rest le) (rest lwr)))]))

; Number Number -> Number
; computes the weekly wage from pay-rate and hours
(define (weekly-wage.v2 pay-rate hours)
  (* pay-rate hours))


;; Exercise 399

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

; consumes a list of names, represented as strings, and a list of phone numbers, also strings.
; It combines those equally long lists into a list of phone records

(check-expect (zip (list "Bob" "Sam" "Rob") (list "123" "321" "222"))
              (list (make-phone-record "Bob" "123")
                    (make-phone-record "Sam" "321")
                    (make-phone-record "Rob" "222")))
                    

(define (zip lon lop)
  (cond
    [(empty? lon) '()]
    [else (cons (make-phone-record (first lon) (first lop))
                (zip (rest lon) (rest lop)))]))