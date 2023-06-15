;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 390

(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path. 


; TOS [List-of Direction] -> Symbol
; produces symbol from tos by given lod

(define tree (make-branch 's (make-branch 's 'a)))

(check-expect (tree-pick 's '()) 's)

(check-error (tree-pick tree '()) "to few directions")

(check-error (tree-pick 's '(left)) "to many directions")
(check-error (tree-pick tree '(right right right)) "to many directions")

(check-expect (tree-pick tree '(right right)) 'a)


(define (tree-pick tos lod)
  (cond
    [(and (symbol? tos) (empty? lod)) tos]
    [(and (symbol? tos) (not (empty? lod))) (error "to many directions")]
    [(and (branch? tos) (empty? lod)) (error "to few directions")]
    [else (tree-pick (if (symbol=? (first lod) 'left)
                         (branch-left tos)
                         (branch-right tos))
                     (rest lod))]))