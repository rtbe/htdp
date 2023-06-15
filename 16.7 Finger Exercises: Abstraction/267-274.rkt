;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 267-274) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 267

(define EXCHANGE-RATE 1.06)

; [List-of Number] -> [List-of Number]
; converts a list of US$ amounts into a list of € amounts
; based on an exchange rate

(check-expect (convert-evro (list 1 1 1 1)) (list 1.06  1.06  1.06  1.06))

(define (convert-evro lon)
  (local (; Number -> Number
          (define (conv-num n) (* n EXCHANGE-RATE)))
    (map conv-num lon)))

; [List-of Number] -> [List-of Number]
; converts a list of Fahrenheit measurements to
; a list of Celsius measurements.

(check-expect (convertFC (list 32 50 140)) (list 0  10  60))

(define (convertFC lon)
  (local (; Number -> Number
          (define (conv-fahr f)  (* (- f 32) (/ 5 9))))
    (map conv-fahr lon)))

; [List-of Posn] -> [List-of [List-of Number]]

(check-expect (translate (list (make-posn 1 1) (make-posn 2 2))) (list (list 1 1) (list 2 2)))

(define (translate pos)
  (local (; Posn -> [List-of Number]
          (define (make-pair p) (list (posn-x p) (posn-y p))))
    (map make-pair pos)))


;; Exercise 268

(define-struct IR [name description acq-price rec-price])
; An IR is a structure:
;   (make-IR String String Number Number)

; [List-of IR] -> [List-of IR]
; sorts a list of inventory records by the difference between the two prices.

(check-expect (sort-lir (list
                         (make-IR "" "" 1 2)
                         (make-IR "" "" 10 20)
                         (make-IR "" "" 11 2)
                         (make-IR "" "" 13 21)
                         (make-IR "" "" 10 29)))
              (list
               (make-IR "" "" 1 2)
               (make-IR "" "" 13 21)
               (make-IR "" "" 11 2)
               (make-IR "" "" 10 20)
               (make-IR "" "" 10 29)))


(define (sort-lir lir)
  (local (; IR -> Number
          ; computes difference between recommendation price and aquired price of given IR
          (define (cmpr-ir ir) (abs (- (IR-rec-price ir) (IR-acq-price ir))))
          
          ; IR IR -> Boolean
          ; compares difference between two IRs prices
          ; and returns true if difference of first IR is less than difference of second IR
          (define (cmpr ir1 ir2) (if
                                  (< (cmpr-ir ir1) (cmpr-ir ir2))
                                  true
                                  false)))
    (sort lir cmpr)))

;; Exercise 269

; Number [List-of IR] -> [List-of IR]
; function consumes a number, ua, and a list of inventory records,
;and it produces a list of all those structures whose sales price is below ua

(check-expect (eliminate-expensive 25 (list
                                       (make-IR "" "" 21 21)
                                       (make-IR "" "" 50 50)
                                       (make-IR "" "" 5 5)
                                       (make-IR "" "" 221 221)
                                       (make-IR "" "" 129 129)))
              (list (make-IR "" "" 21 21)
                    (make-IR "" "" 5 5)))
                    
(define (eliminate-expensive ua loi)
  (local (; IR -> Boolean
          ; returns true if acq-price of given IR is less than ua
          (define (filter-ir ir) (if (< (IR-acq-price ir) ua)
                                     true
                                     false))
          ) (filter filter-ir loi)))

; [List-of IR] -> [List-of IR]
; consumes the name of an inventory item, called ty, and a list of inventory records and which produces
; a list of inventory records that do not use the name ty

(check-expect (recall "loh" (list
                                       (make-IR "loh" "" 21 21)
                                       (make-IR "doh" "" 50 50)
                                       (make-IR "boh" "" 5 5)
                                       (make-IR "joh" "" 221 221)
                                       (make-IR "loh" "" 129 129)))
              (list  (make-IR "doh" "" 50 50)
                     (make-IR "boh" "" 5 5)
                     (make-IR "joh" "" 221 221)))

(define (recall ty loi)
  (local (; IR -> Boolean
          ; returns true if name of IR is not ty
          (define (filter-ir ir) (if (not (string=? ty (IR-name ir)))
                                     true
                                     false)))
    (filter filter-ir loi)))

; [List-of String] [List-of String] -> [List-of String]
; consumes two lists of names and selects all
; those from the second one that are also on the first

(check-expect (selection
               (list "Vasya" "Petya" "Kolya" "Dima" "Sasha")
               (list "John" "Bob" "Dima" "Leon" "Vasya"))
              (list "Dima" "Vasya"))

(define (selection lon1 lon2)
  (local (; String -> Boolean
          ; checks if given string name is in lon1
          (define (select name)(member? name lon1)))
    (filter select lon2)))

;; Exercise 270

; Number -> [List-of Number]
; creates the list (list 0 ... (- n 1)) for any natural number n
(check-expect (build-list-asc 22)
(list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21))

(define (build-list-asc n)
  (local (; Number -> Number
          (define (f x) x))
    (build-list n f)))

; Number -> [List-of Number]
; creates the list (list 1 ... n) for any natural number n
(check-expect (build-list-asc-plus-one 22)
(list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22))

(define (build-list-asc-plus-one n)
  (local (; Number -> Number
          (define (f x) (+ 1 x)))
    (build-list n f)))

; Number -> [List-of Number]
; creates the list (list 1 1/2 ... 1/n) for any natural number n
(check-expect (build-list-divide-one 5)
(list 1 (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5)))

(define (build-list-divide-one n)
  (local (; Number -> Number
          (define (f x) (/ 1 (+ 1 x))))
    (build-list n f)))


; Number -> [List-of Number]
; creates the list of the first n even numbers
(check-expect (build-list-even 5)
(list 2 4 6 8 10))

(define (build-list-even n)
  (local (; Number -> Number
          (define (f x) (* 2 (+ 1 x))))
    (build-list n f)))

; Number -> [List-of [List-of Number]]
; creates diagonal squares of 0s and 1s:
(check-expect (identityM 3)
(list (list 1 0 0)
      (list 0 1 0)
      (list 0 0 1)))

(define (identityM n)
  (local ((define (make-row pos len)
            (cond
              [(= len n) '()]
              [else (cons
                     (if (= pos len) 1 0)
                     (make-row pos (+ len 1)))]))
          
          (define (f x) (make-row x 0)))
    (build-list n f)))

; [Number -> Number] Number -> [List-of Number] 
(define (tabulate f n) (build-list f n)) 



;; Exercise 271

; String [List-of String] -> Boolean
; consumes a name and a list of names. It determines whether
; any of the names on the latter are equal to or an extension of the former.
(check-expect (find-name "Vas" (list "Vasya" "Petya" "Kolya" "Zhenya")) #t)
(check-expect (find-name "Bas" (list "Vasya" "Petya" "Kolya" "Zhenya")) #f)

(define (find-name name lon)
  (local (; String -> Boolean
          (define (f n) (string-contains-ci? name n)))
    (ormap f lon)))

; [List-of String] -> Boolean
; consumes a name and a list of names. It determines whether
; all names on a list of names that start with the letter "a".
(check-expect (all-names-start-with-a? (list "Vasya" "Petya" "Kolya" "Zhenya")) #f)
(check-expect (all-names-start-with-a? (list "Ally" "Ann")) #t)

(define (all-names-start-with-a? lon)
  (local (; String -> Boolean
          (define (f n) (string=? "a" (string-downcase (string-ith n 0)))))
    (andmap f lon)))


; i should use andmap since it checks for
; whether some condition holds for all items of given list

; Number [List-of String] -> Boolean
; ensures that no name on some list exceeds a given width
(check-expect (all-strings-smaller-than-length? 2 (list "Vasya" "Petya" "Kolya" "Zhenya")) #f)
(check-expect (all-strings-smaller-than-length? 10 (list "Ally" "Ann")) #t)

(define (all-strings-smaller-than-length? len lon)
  (local (; String -> Boolean
          (define (f n) (<= (string-length n) len)))
    (andmap f lon)))

;; Exercise 272

; [List-of Number] [List-of Number] -> [List-of Number]
(check-expect (append-from-fold (list 1 2 3) (list 4 5 6 7 8))
              (append (list 1 2 3) (list 4 5 6 7 8)))

(define (append-from-fold l1 l2)
    (foldr cons l2 l1))

; [List-of Number] -> Number
(check-expect (sum-list (list 1 2 3 4 5)) 15)

(define (sum-list l1)
  (local (; Number Number -> Number
          (define (f x y) (+ x y)))
    (foldr f 0 l1)))

; [List-of Number] -> Number
(check-expect (product-list (list 1 2 3 4 5)) 120)

(define (product-list l1)
  (local (; Number Number -> Number
          (define (f x y) (* x y)))
    (foldr f 1 l1)))

; [List-of Image] -> Image
(define (compose-horiz loi)
    (foldr beside empty-image loi))

(compose-horiz (list (rectangle 20 20 "solid" "green")
                     (rectangle 20 20 "solid" "yellow")
                     (rectangle 20 20 "solid" "red")))

; [List-of Image] -> Image
(define (compose-vert loi)
    (foldl above empty-image loi))

(compose-vert (list (rectangle 20 20 "solid" "green")
                     (rectangle 20 20 "solid" "yellow")
                     (rectangle 20 20 "solid" "red")))

;; Exercise 273

; [X Y] [X -> Y] [List-of X] -> [List-of Y]
(check-expect (map-from-fold add1 (list 1 2 3)) (map add1 (list 1 2 3)))

(define (map-from-fold f lon)
  (local (; [X Y] X [List-of Y] -> [List-of Y]
          (define (fn arg1 arg2) (cons (f arg1) arg2)))
    (foldr fn '() lon)))

;; Exercise 274

; [List-of 1String] -> [List-of 1String]
(check-expect (suffixes (list "a" "b" "c" "d"))
              (list (list "a" "b" "c" "d")
                    (list "b" "c" "d")
                    (list "c" "d")
                    (list "d")))

(define (suffixes l)
  (local (;
          (define (f x y) (list x y)))
    (foldr f '() l)))
