;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 316-321) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
          
; An Atom is one of: 
; – Number
; – String
; – Symbol


; S-expr Symbol -> N 
; counts all occurrences of sy in sexp

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

(define (count sexp sy)
 (cond
   [(atom? sexp) (count-atom sexp sy)]
   [else (count-sl sexp sy)]))
 
; SL Symbol -> N 
; counts all occurrences of sy in sl 
(define (count-sl sl sy)
  (cond
    [(empty? sl) 0]
    [else (+ (count (first sl) sy) (count-sl (rest sl) sy))]))
 
; Atom Symbol -> N 
; counts all occurrences of sy in at 
(define (count-atom at sy)
  (cond
    [(number? at) 0]
    [(string? at) 0]
    [(symbol? at) (if (symbol=? at sy) 1 0)]))

;; Exercise 316

; Atom -> Boolean
; defines if an is a atom
(define (atom? a)
  (if (or (number? a)
          (string? a)
          (symbol? a)) #t #f))

;; Exercise 317

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp

(check-expect (count* 'world 'hello) 0)
(check-expect (count* '(world hello) 'hello) 1)
(check-expect (count* '(((world) hello) hello) 'hello) 2)

(define (count* sexp sy)
  (local (; SL Symbol -> N 
          ; counts all occurrences of sy in sl 
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else (+ (count (first sl) sy) (count-sl (rest sl)))]))
 
          ; Atom Symbol -> N 
          ; counts all occurrences of sy in at 
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))

;; Exercise 318

; S-expr -> Number
; consumes an S-expression and determines its depth.
; An Atom has a depth of 1.
; The depth of a list of S-expressions is the maximum depth of its items plus 1

(check-expect (depth 'world ) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '(((world) hello))) 4)
(check-expect (depth 10) 1)
(check-expect (depth '( "sd" 34)) 2)
(check-expect (depth '((21 12))) 3)
(check-expect (depth '( "sd" 34 (12 er))) 3)

(define (depth sexp)
  (local (; SL -> N 
          ; defines depth of given SL
          (define (depth-sl sl)
            (cond
              [(empty? sl) 1]
              [else (max (depth (first sl)) (depth-sl (rest sl)))]))) ; maximum depth of its items
    (cond
      [(atom? sexp) 1] ; An Atom has a depth of 1
      [else (add1 (depth-sl sexp))]))) ; The depth of a list of S-expressions is the maximum depth of its items plus 1

;; Exercise 319

; S-expr Symbol Symbol -> S-expr
; consumes an S-expression s and two symbols, old and new.
; The result is like s with all occurrences of old replaced by new

(check-expect (substitute 'world 'world 'new) 'new)
(check-expect (substitute '(world hello) 'world 'hello) '(hello hello))
(check-expect (substitute '(((world) hello)) 'world 'hello) '(((hello) hello)))
(check-expect (substitute 'as 'as 'ad) 'ad)
(check-expect (substitute '(as 1 2) 'as 'ad) '(ad 1 2))
(check-expect (substitute '(as 1 2 (as)) 'as 'ad) '(ad 1 2 (ad)))

(define (substitute sexp old new)
  (local (
         (define (traverse-sl sl)
           (cond
             [(empty? sl) sl]
             [else (cons (substitute (first sl) old new) (traverse-sl (rest sl)))]))
          
          ; Atom -> Atom
          ; replaces symbol in a from old to new
          ; otherwise return original value
          (define (replace-atom a)
            (cond
              [(symbol? a) (if (symbol=? a old) new a)]
              [else a])))
    (cond
      [(atom? sexp) (replace-atom sexp)]
      [else (traverse-sl sexp)])))

;; Exercise 320

; An S-expr is one of: 
; – String
; – Number
; – Symbol
; – [List-of S-expr]

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp

(check-expect (count** 'world 'hello) 0)
(check-expect (count** '(world hello) 'hello) 1)
(check-expect (count** '(((world) hello) hello) 'hello) 2)

(define (count** sexp sy)
            (cond
              [(number? sexp) 0]
              [(string? sexp) 0]
              [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
              [else (cond
                      [(empty? sexp) 0]
                      [else (+ (count** (first sexp) sy) (count** (rest sexp) sy))])]))


; An S-expr is one of: 
; – String
; – Number
; – Symbol
; – '()
; – (cons S-expr SL)

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp

(check-expect (count*** 'world 'hello) 0)
(check-expect (count*** '(world hello) 'hello) 1)
(check-expect (count*** '(((world) hello) hello) 'hello) 2)

(define (count*** sexp sy)
  (cond
    [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
    [(empty? sexp) 0]
    [else (foldl (lambda (x y) (+ (count*** x sy) y)) 0 sexp)]))

; Exercise 321

; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
          
; An Atom is any Item