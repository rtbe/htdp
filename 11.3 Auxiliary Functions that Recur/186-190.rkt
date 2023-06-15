;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; List-of-numbers -> List-of-numbers
; produces a sorted version of l

(check-expect (sort> '()) '())
(check-expect (sort> (list 3 2 1)) (list 3 2 1))
(check-expect (sort> (list 1 2 3)) (list 3 2 1))
(check-expect (sort> (list 12 20 -5))
              (list 20 12 -5))

(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l

(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))

(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; Exercise 186

; List-of-numbers -> Boolean
; are temperatures sorted in descending order
(check-expect (sorted>? (cons 3 (cons 2 (cons 1 '())))) #true)
(check-expect (sorted>? (cons 2 (cons 1 '()))) #true)
(check-expect (sorted>? (cons 1 (cons 2 (cons 3 '())))) #false)
(check-expect (sorted>? (cons 2 (cons 3 '()))) #false)

(define (sorted>? lon)
  (cond
   [(or (empty? lon) (empty? (rest lon))) #true]
   [else (and (>= (first lon) (second lon)) (sorted>? (rest lon)))]))

(check-satisfied (sort> '()) sorted>?)
(check-satisfied (sort> (list 3 2 1)) sorted>?)
(check-satisfied (sort> (list 1 2 3)) sorted>?)
(check-satisfied (sort> (list 12 20 -5)) sorted>?)

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))

(check-expect (sort>/bad '()) '())

; We can not use check-satisfied because
; we have not way to compare result of first expression with result of second.
; Check-satisfied is way to generic;
; We can not pass the same arument to both of functions in check-satisfied

; Exercise 187

(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points

(define gp1 (make-gp "vasya" 1))
(define gp2 (make-gp "petya" 2))
(define gp3 (make-gp "kolya" 3))

; GamePlayer GamePlayer -> Boolean
; compares if 1gp score more than 2gp score
(check-expect (gp>? gp1 gp2) #false)
(define (gp>? gp1 gp2)
  (> (gp-score gp1) (gp-score gp2)))

; List-of-gamePlayers is one of:
; - '()
; - (cons GamePlayer List-of-gamePlayers)
; interpretation is a list of GamePlayers

; List-of-gamePlayers -> List-of-gamePlayers
; returns sorted in descended order list of game players

(check-expect (gpSort> (list gp1 gp2 gp3)) (list gp3 gp2 gp1))

(define (gpSort> logp)
  (cond
    [(empty? logp) '()]
    [else (insertGp (first logp) (gpSort>(rest logp)))]))

; GamePlayer List-of-gamePlayers -> List-of-gamePlayers
; inserts gp into sorted list of game players
(check-expect (insertGp gp1 (list gp3 gp2)) (list gp3 gp2 gp1))

(define (insertGp gp logp)
  (cond
    [(empty? logp) (cons gp '())]
    [else (if
           (gp>? gp (first logp))
           (cons gp logp)
           (cons (first logp) (insertGp gp (rest logp))))]))


; Exercise 188

(define-struct email [from date message])
; An Email Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time

(define email1 (make-email "v" 1 "v"))
(define email2 (make-email "va" 2 "va"))
(define email3 (make-email "vas" 3 "vas"))


; List-of-emails is one of:
; - '()
; - (cons Email List-of-emails)


; Email Email -> Boolean
; compares email1 with email2 and returns #true
; if date of email1 is more than date in email2
(check-expect (emailDate>? email1 email2) #false)
(check-expect (emailDate>? email2 email1) #true)
(define (emailDate>? email1 email2)
  (> (email-date email1) (email-date email2)))


; Email List-of-emails -> List-of-emails
; inserts email into sorted list of emails loe in ascending order

(check-expect (insert-email-by-date email1 (list email2 email3)) (list email1 email2 email3))
(check-expect (insert-email-by-date email3 (list email1 email2)) (list email1 email2 email3))

(define (insert-email-by-date email loe)
  (cond
   [(empty? loe) (cons email loe)]
   [else (if
          (emailDate>? (first loe) email)
          (cons email loe)
          (cons (first loe) (insert-email-by-date email (rest loe))))]))

; List-of-emails -> List-of-emails
; returns sorted list of emails in ascending dates order from provided loe

(check-expect
 (loe-sort-date< (list email3 email2 email1))

 (list email1 email2 email3))
(define (loe-sort-date< loe)
  (cond
   [(empty? loe) '()]
   [else (insert-email-by-date (first loe) (loe-sort-date< (rest loe)))]))


; Email Email -> Boolean
; compares names in email1 and email2
; if name in email1 is more than in email2 returns #true

(check-expect (email-from>? email1 email3) #false)
(check-expect (email-from>? email3 email1) #true)

(define (email-from>? email1 email2)
  (not (string<? (email-from email1) (email-from email2))))


; Email List-of-emails -> List-of-emails
; inserts email into sorted list of emails loe in descending order of from field

(check-expect (insert-loe-by-from email3 (list email2 email1)) (list email3 email2 email1))
(check-expect (insert-loe-by-from email1 (list email3 email2)) (list email3 email2 email1))

(define (insert-loe-by-from email loe)
  (cond
   [(empty? loe) (cons email loe)]
   [else (if (email-from>? email (first loe))
             (cons email loe)
             (cons (first loe) (insert-loe-by-from email (rest loe))))]))


; List-of-emails -> List-of-email
; returns sorted list of emails in descending from order from provided loe

(check-expect (loe-sort-from> (list email2 email3 email1)) (list email3 email2 email1))

(define (loe-sort-from> loe)
  (cond
    [(empty? loe) '()]
    [else (insert-loe-by-from (first loe) (rest loe))]))


; Email List-of-emalis -> List-of-emails

; Exercise 189

; Number List-of-numbers -> Boolean

(check-expect (search 1 (list 3 2 1)) #true)
(check-expect (search 5 (list 3 2 1)) #false)

(define (search n alon)
  (cond
    [(empty? alon) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))

; Number List-of-numbers -> Boolean

(check-expect (search-sorted> 1 (list 3 2 1)) #true)
(check-expect (search-sorted> 1 (list 3 2 0)) #false)
(check-expect (search-sorted> 5 (list 3 2 1)) #false)

(define (search-sorted> n alon)
  (cond
    [(empty? alon) #false]
    [(< (first alon) n) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))


; Exercise 190

; List-of-1Strings is one of:
; - '()
; - (cons 1String List-of-1Strings)
; interpretation is a list of 1String

; L1S is one of:
; - '(cons List-of-1String L1S)

; List-of-1Strings -> L1S

(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a")) (list (list "a")))
(check-expect (prefixes (list "a" "b")) (list (list "a") (list "a" "b")))
(check-expect (prefixes (list "a" "b" "c"))
              (list (list "a") (list "a" "b") (list "a" "b" "c")))

(define (prefixes los)
  (cond
   [(empty? los) '()]
   [else (list los (rest los))]))

