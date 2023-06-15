;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 258-261) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 258

; Image Polygon -> Image 
; adds a corner of p to img
(define (render-poly img p)
  (local (; Image NELoP -> Image
          ; connects the Posns in p in an image
          (define (connect-dots img p)
            (cond
              [(empty? (rest p)) img]
              [else (render-line (connect-dots img (rest p))
                                 (first p)
                                 (second p))]))

          ; Image Posn Posn -> Image 
          ; draws a red line from Posn p to Posn q into im
          (define (render-line im p q)
            (scene+line
             im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

          ; Polygon -> Posn
          ; extracts the last item from p
          (define (last p)
            (cond
              [(empty? (rest (rest (rest p)))) (third p)]
              [else (last (rest p))])))
    (connect-dots img p)))

;; Exercise 259

;; Exercise 260

; Nelon -> Number
; takes R as comparison function and produces result number
(define (extract R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (local ((define extract-in-rest (extract R (rest l))))
       (if (R (first l)
              extract-in-rest)
           (first l)
           extract-in-rest))]))

; Nelon -> Number
; determines the smallest 
; number on l

(check-expect (inf-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
     12 11 10 9 8 7 6 5 4 3 2 1)) 1)

(check-expect (inf-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25)) 1)

(define (inf-1 l) (extract < l))

; Nelon -> Number
; determines the largest 
; number on l

(check-expect (sup-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
     12 11 10 9 8 7 6 5 4 3 2 1)) 25)

(check-expect (sup-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
    17 18 19 20 21 22 23 24 25)) 25)

(define (sup-1 l) (extract > l))

;; Exercise 261

(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (local ((define extract1-rest (extract1 (rest an-inv))))
     (cond
       [(<= (IR-price (first an-inv)) 1.0)
        (cons (first an-inv) extract1-rest)]
       [else extract1-rest]))]))

; No this does not help to inscease the speed of computing the function,
; because the result of extract1 is already computed only once in each part of the cond

