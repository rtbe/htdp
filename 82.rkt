;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |82|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
; OneLetter is a one letter string
; - 1String through "a" to "z"
; - #false

(define-struct three-letter-word [first second third])
; ThreeLetterWord is a structure:
;   (make-three-letter-word "a" false "z")
; interpretation a word that consists of three OneLetter
(define tlw1 (make-three-letter-word "a" false "z"))
(define tlw2 (make-three-letter-word "a" false "b"))
(define tlw3 (make-three-letter-word "a" false false))

; OneLetter -> Bool
; compares l1 with l2, if they are the same returns true
(check-expect (compare-letter "a" "a") true)
(check-expect (compare-letter "a" "z") false)
(check-expect (compare-letter "a" false) false)
(check-expect (compare-letter false "z") false)
(check-expect (compare-letter false false) true)
(define (compare-letter l1 l2)
  (cond
    [(and (string? l1) (string? l2)) (string=? l1 l2)]
    [(and (boolean? l1) (boolean? l2)) (and (false? l1) (false? l2))]
    [(and (boolean? l1) (not (boolean? l2))) false]
    [(and (not (boolean? l1)) (boolean? l2)) false]
    ))

; ThreeLetterWord, ThreeLetterWord -> ThreeLetterWord
; It produces a word that indicates where the given ones agree and disagree.
; The function retains the content of the structure fields if the two agree
; otherwise it places #false in the field of the resulting word
(check-expect (compare-word tlw1 tlw1) tlw1)
(check-expect (compare-word tlw1 tlw2) tlw3)
(define (compare-word tlw1 tlw2)
  (make-three-letter-word
   (if
    (compare-letter (three-letter-word-first tlw1) (three-letter-word-first tlw2))
    (three-letter-word-first tlw1)
    false)
   
    (if (compare-letter (three-letter-word-second tlw1) (three-letter-word-second tlw2))
    (three-letter-word-second tlw1)
    false)
   
    (if (compare-letter (three-letter-word-third tlw1) (three-letter-word-third tlw2))
    (three-letter-word-third tlw1)
    false)))