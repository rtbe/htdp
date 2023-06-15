;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 65-66) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct movie [title producer year])
(define m (make-movie "Titanic" "Vasya Pupkin" "1999"))
(movie? m)
(movie-title m)
(movie-producer m)
(movie-year m)

(define-struct person [name hair eyes phone])
(define pers (make-person "Vasya" "Blond" "Blue" "123123"))
(person? pers)
(person-name pers)
(person-hair pers)
(person-eyes pers)
(person-phone pers)

(define-struct pet [name number])

(define-struct CD [artist title price])

(define-struct sweater [material size producer])