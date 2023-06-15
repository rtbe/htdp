;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 322-327) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define bt1 (make-node
  15
  'd
  NONE
  (make-node
    24 'i NONE NONE)))

(define bt2 (make-node
  15
  'd
  (make-node
    87 'h NONE NONE)
  NONE))

(define bt3 (make-node
  15
  'd
  NONE
  (make-node
    87 'h NONE NONE)))

;; Exercise 322

; Number BT  -> Boolean
; determines whether a given number occurs in some given BT

(check-expect (contains-bt? 87 bt1) #f)
(check-expect (contains-bt? 87 bt2) #t)

(define (contains-bt? n bt)
  (cond
    [(no-info? bt) #f]
    [else (or (= (node-ssn bt) n)
              (contains-bt? n (node-left bt))
              (contains-bt? n (node-right bt)))]))

;; Exercise 323

; Number BT -> [Maybe String]
; If the tree contains a node structure whose ssn field is n,
; the function produces the value of the name field in that node.
; Otherwise, the function produces #false

(check-expect (search-bt 87 bt1) #f)
(check-expect (search-bt 87 bt2) 'h)

(define (search-bt n bt)
  (cond
    [(no-info? bt) #f]
    [(= (node-ssn bt) n) (node-name bt)]
    [else (if (contains-bt? n (node-left bt))
              (search-bt n (node-left bt))
              (search-bt n (node-right bt)))]))

(check-expect (search-bt* 87 bt1) #f)
(check-expect (search-bt* 87 bt2) 'h)
(check-expect (search-bt* 87 bt3) 'h)

(define (search-bt* n bt)
  (cond
    [(no-info? bt) #f]
    [(= (node-ssn bt) n) (node-name bt)]
    [else (if (not (boolean? (search-bt* n (node-left bt))))
              (search-bt* n (node-left bt))
              (search-bt* n (node-right bt)))]))


;; Exercise 324

; BT -> [List-of Number]
; consumes a binary tree and produces the sequence of all the ssn numbers
; in the tree as they show up from left to right when looking at a tree drawing

(check-expect (inorder bt1) '(15 24))
(check-expect (inorder bt2) '(87 15))

(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else (append (inorder (node-left bt))
                  (list (node-ssn bt))
                  (inorder (node-right bt)))]))

;; Exercise 325

; MN is short for Maybe Name is one of:
; - String
; - No-info 

; Number BST -> MN
; consumes a number n and a BST. If the tree contains a node whose ssn field is n,
; the function produces the value of the name field in that node.
; Otherwise, the function produces NONE

(define bst (make-node
                             30
                             'd
                             (make-node
                              20 'i NONE NONE)
                             (make-node
                              40 'j NONE
                              (make-node
                              45 'k NONE NONE))))

(check-expect (search-bst 45 bst) 'k)
(check-expect (search-bst 20 bst) 'i)
(check-expect (search-bst 40 bst) 'j)
(check-expect (search-bst 50 bst) NONE)

(define (search-bst n bst)
  (cond
    [(no-info? bst) NONE]
    [else (if (= (node-ssn bst) n)
              (node-name bst)
              (if (< (node-ssn bst) n)
                  (search-bst n (node-right bst))
                  (search-bst n (node-left bst))))]))


;; Exercise 326

; BST Number Symbol -> BST
; consumes a BST B, a number N, and a symbol S.
; It produces a BST that is just like B and that in place of
; one NONE subtree contains the node structure

(check-expect (create-bst bst 10 's)
                          (make-node
                             30
                             'd
                             (make-node
                              20 'i (make-node 10 's NONE NONE) NONE)
                             (make-node
                              40 'j NONE
                              (make-node
                              45 'k NONE NONE))))

(define (create-bst b n s)
  (cond
    [(no-info? b) (make-node n s NONE NONE)]
    [else (if (< (node-ssn b) n)
              (make-node (node-ssn b) (node-name b) (node-left b) (create-bst (node-right b) n s))
              (make-node (node-ssn b) (node-name b) (create-bst (node-left b) n s) (node-right b)))]))
    
;; Exercise 327

(check-expect 
  (create-bst-from-list '((10 x))) 
  (make-node 10 'x NONE NONE))
(check-expect 
  (create-bst-from-list '((10 x) (20 x) (2 x) (7 x))) 
  (make-node 10 'x 
    (make-node 2 'x
      NONE 
      (make-node 7 'x NONE NONE))
    (make-node 20 'x NONE NONE)))

; [List-of [List Number Symbol]] -> BST
; consumes a list of numbers and names and produces a BST by repeatedly applying create-bst
(define (create-bst-from-list l)
  (foldl (lambda (x y) (create-bst y (first x) (second x))) NONE l))

; Because root of the tree will be first item of the given list and we moves from left to right futher
; We can reverse given list or use foldr instead