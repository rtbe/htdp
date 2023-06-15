;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |387|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 387

; Pair is (list Symbol Number)

;; [List-of Symbol] [List-of Numbers] -> [List-of Pair]
; produces all possible ordered pairs of symbols and numbers

(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))

(define (cross los lon)
  (local (; Symbol [List-of Number] -> [List-of Pair]
          ; produces all possible ordered pairs of given symbol and numbers
          (define (val s lon)
            (cond
              [(empty? lon) '()]
              [else (cons (list s (first lon))
                          (val s (rest lon)))])))
    (cond
      [(empty? los) '()]
      [else (append (val (first los) lon)
                    (cross (rest los) lon))])))
