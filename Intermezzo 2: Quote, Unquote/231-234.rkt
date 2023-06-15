;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 231-234) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/web-io)

;; Exercise 231

(check-expect '(1 "a" 2 #false "c") (list 1 "a" 2 #false "c"))
(check-expect '(1 "a" 2 #false "c") (cons 1 (cons "a" (cons 2 (cons #false (cons "c" '()))))))

(check-expect '() '())


(check-expect
 '(("alan" 1000)
  ("barb" 2000)
  ("carl" 1500))
 (list (list "alan" 1000) (list "barb" 2000) (list "carl" 1500)))
(check-expect
 '(("alan" 1000)
  ("barb" 2000)
  ("carl" 1500))
 (cons (cons "alan" (cons 1000 '()))
       (cons (cons "barb" (cons 2000 '()))
             (cons (cons "carl" (cons 1500 '())) '()))))

;; Exercise 232

(check-expect `(1 "a" 2 #false 3 "c")
              (list 1 "a" 2 #false 3 "c"))

(check-expect
 `(("alan" ,(* 2 500))
   ("barb" 2000)
   (,(string-append "carl" " , the great") 1500)
   ("dawn" 2300))
  (list
   (list "alan" 1000)
   (list "barb" 2000)
   (list "carl , the great" 1500)
   (list "dawn" 2300)))

(define title "ratings")
(check-expect
 `(html
   (head
     (title ,title))
   (body
     (h1 ,title)
     (p "A second web page")))
 (list 'html
       (list 'head (list 'title "ratings"))
       (list 'body
             (list 'h1 "ratings")
             (list 'p "A second web page"))))

;; Exercise 233

(check-expect
 `(0 ,@'(1 2 3) 4)
 (list 0 1 2 3 4))

(check-expect
 `(("alan" ,(* 2 500))
  ("barb" 2000)
  (,@'("carl" " , the great")   1500)
  ("dawn" 2300))
 (list
  (list "alan" 1000)
  (list "barb" 2000)
  (list "carl" " , the great" 1500)
  (list "dawn" 2300)))

; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))

; Number -> ... nested list ...
; creates a cell for an HTML table from a number 
(define (make-cell n)
  `(td ,(number->string n)))

(check-expect
 `(html
   (body
     (table ((border "1"))
       (tr ((width "200"))
         ,@(make-row '( 1  2)))
       (tr ((width "200"))
         ,@(make-row '(99 65))))))
(list
 'html
 (list
  'body
  (list
   'table
   (list (list 'border "1"))
   (list
    'tr
    (list (list 'width "200"))
    (list 'td "1")
    (list 'td "2"))
   (list
    'tr
    (list (list 'width "200"))
    (list 'td "99")
    (list 'td "65"))))))

;; Exercise 234

; List-of-ranked-song-titles -> ... nested list ...
; creates a row for an HTML table from l
(define (make-rows-st lrst)
  (cond
    [(empty? lrst) '()]
    [else (cons (make-row-st (first lrst))
                (make-rows-st (rest lrst)))]))

; List-of-ranked-song-titles -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row-st lrst)
  (cond
    [(empty? lrst) '()]
    [else `(tr ,(make-cell-number (first lrst))
               ,(make-cell-title (second lrst)))]))

; ranked-song-title -> ... nested list ...
; creates a cell for an HTML table from a number 
(define (make-cell-number num)
  `(td ,(number->string num)))

; ranked-song-title -> ... nested list ...
; creates a cell for an HTML table from a number 
(define (make-cell-title title)
  `(td ,title))

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

; List-of-ranked-song-titles -> list representation of an HTML table
(define (make-ranking los)
 `(html
   (body
     (table ((border "1"))
       ,@(make-rows-st los)))))


; List-of-strings -> ... nested list ...
; determines the rankings from a lost of strings
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-strings -> ... nested list ...
; determines the rankings from a lost of strings
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

(show-in-browser (make-ranking (ranking one-list)))