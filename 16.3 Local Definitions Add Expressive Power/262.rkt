;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |262|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 262

; Number -> [List-of [List-of Number]]

(check-expect (identityM 1)
              (list (list 1)))

(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

(define (identityM n)
  (local (; len of matrix row
          ; (define len n)

          ; Number Number -> [List-of Number]
          ; returns a list of num elements with 1 at pos
          (define (make-row num pos)
            (cond
              [(= num 0) '()]
              [else (cons (if (= num pos) 1 0)
                          (make-row (- num 1) pos))]))
          
          ; Number -> [List-of [List-of Number]
          ; returns a matrix
          (define (make-matrix num) 
           (cond
             [(= num 0) '()]
             [else (cons (make-row n num) (make-matrix (- num 1)))])))
    (make-matrix n)))
