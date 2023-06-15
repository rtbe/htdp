;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |80|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct movie [title director year])

; Movie -> String
; returns description of movie
(define (movie-descr m) (... (movie-title m) ... (movie-director) ... (movie-year))

(define-struct pet [name number])

(define-struct CD [artist title price])

(define-struct sweater [material size color])