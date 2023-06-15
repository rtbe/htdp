;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |81|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct time-since-midnight [hours minutes seconds])
; A Time-since-midnight is a structure:
;   (make-time-scince-midnight Number Number Number)
; interpretation time sinсe midnight

; Time-since-midnight -> Number
; produces number of seconds that have passed since midnight
(check-expect (time->seconds (make-time-since-midnight 0 0 10)) 10)
(check-expect (time->seconds (make-time-since-midnight 0 1 0)) 60)
(check-expect (time->seconds (make-time-since-midnight 1 0 0)) 3600)
(check-expect (time->seconds (make-time-since-midnight 12 30 2)) 45002)
(define (time->seconds t)
  (+
   (time-since-midnight-seconds t)
   (* (time-since-midnight-minutes t) 60)
   (* (time-since-midnight-hours t) 3600)))