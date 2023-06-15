;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |76|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct movie [title producer year])
; Movie is (make-movie String String Number)

(define-struct person [name hair eyes phone])
; Person is (make-person String String String String)

(define-struct pet [name number])
; Pet is (make-pet String Number)

(define-struct CD [artist title price])
; CD is (make-cd String String Number)

(define-struct sweater [material size producer])
; Sweater is (make-sweater String, String, String)