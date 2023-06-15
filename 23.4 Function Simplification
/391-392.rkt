;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 391-392) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 391

; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end

(check-expect (replace-eol-with '() '(a b)) '(a b))
(check-expect (replace-eol-with '(a b) '()) '(a b))
(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with
                (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))

(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [(empty? end) front]
    [else
     (cons (first front)
           (replace-eol-with (rest front) end))]))

;; Exercise 392

(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path.

(define LEFT 'left)
(define RIGHT 'right)

; TOS [List-of Direction] -> Symbol
; produces symbol from tos by given lod

(define tree (make-branch 's (make-branch 's 'a)))

(check-expect (tree-pick 's '()) 's)

(check-error (tree-pick tree '()) "to few directions")

(check-error (tree-pick 's (list LEFT)) "to many directions")
(check-error (tree-pick tree (list RIGHT RIGHT RIGHT)) "to many directions")

(check-expect (tree-pick tree (list RIGHT RIGHT)) 'a)


(define (tree-pick tos lod)
  (cond
    [(symbol? tos) (if (empty? lod)
                       tos
                       (error "to many directions"))]
    [else (if (empty? lod)
          (error "to few directions")
          (tree-pick (if (symbol=? (first lod) LEFT)
                         (branch-left tos)
                         (branch-right tos))
                     (rest lod)))]))