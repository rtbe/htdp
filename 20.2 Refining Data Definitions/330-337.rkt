;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 330-337) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/abstraction)

; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String. 

;; Exercise 330

(define dt.1
  '(("part1" "part2" "part3")
    "read!"
    (("hang" "draw")
     ("read!"))))

(define dt.2
  (list
   (list "part1" "part2" "part3")
   "read!"
   (list (list "hang" "draw" ) (list "read!"))))



(define dt.3
  (cons (cons "part1" (cons "part2" (cons "part3" '())))
        (cons "read!"
              (cons (cons (cons "hang" (cons "draw" '())) (cons (cons "read!" '()) '())) '()))))
   
(check-expect dt.1 dt.3)

;; Exercise 331


; Dir.v1 -> Number
; determines how many files a given Dir.v1 contains

(check-expect (how-many dt.2) 7)

(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [else (+ (if (string? (first dir)) 1 (how-many (first dir)))
             (how-many (rest dir)))]))


(define-struct dir [name content])

; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String. 

;; Exercise 332

(define dt.4
  (make-dir
    "TS"
    (list
      (make-dir "Text" '("part1" "part2" "part3"))
      "read!"
      (make-dir "Libs"
        (list
          (make-dir "Code" '("hang" "draw"))
          (make-dir "Docs" '("read!"))
          )))))

(define dt.5 (make-dir "TS" (list (make-dir "Text" (list "part1" "part2" "part3"))
                     "read!"
                     (make-dir "Libs" (list (make-dir "Code" (list "hang" "draw" ))
                                            (make-dir "Docs" (list "read!")))))))
(check-expect dt.4 dt.5)

;; Exercise 333

; Dir.v2 -> Number
; determines how many files a given Dir.v2 contains

(check-expect (how-many-files-in-lofd (make-dir "Libs" (list (make-dir "Code" (list "hang" "draw" ))
                                            (make-dir "Docs" (list "read!"))))) 3)


;; Exercise 334

(define-struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String)

(define-struct dir.v3 [name dir* file*])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)

;; Exercise 335

(define dt.6
  (make-dir.v3 
    "TS"
    (list 
      (make-dir.v3
        "Text"
        '()
        (list
          (make-file "part1" 99 "")
          (make-file "part2" 52 "")
          (make-file "part3" 17 "")
          ))
      (make-dir.v3
        "Libs"
        (list
          (make-dir.v3 "Code" '() (list (make-file "hang" 8 "") (make-file "draw" 2 "")))
          (make-dir.v3 "Docs" '() (list (make-file "read!" 19 "")))
          )
        '()
        ))
    (list (make-file "read!" 10 ""))
    ))


(define dt.7 (make-dir.v3 "TS" (list (make-dir.v3 "Text"  '() (list (make-file "part1" 99 "")
                                                                    (make-file "part2" 52 "")
                                                                    (make-file "part3" 17 "")))
                                     (make-dir.v3 "Libs" (list (make-dir.v3 "Code" '() (list (make-file "hang" 8 "")
                                                                                             (make-file "draw" 2 "")))
                                                               (make-dir.v3 "Docs" '() (list (make-file "read!" 19 "")))) '()))
                          (list (make-file "read!" 10 ""))))
                        

(check-expect dt.6 dt.7)

;; Exercise 336

; Dir.v3 -> N
; Counts files withing a directory tree
(check-expect (how-many.v3 dt.6) 7)
(check-expect (how-many.v4 dt.6) 7)

(define (how-many.v3 dir)
  (+ (length (dir.v3-file* dir))
     (for/sum ([i (dir.v3-dir* dir)]) (how-many.v3 i))))

(define (how-many.v4 dir)
  (+ (length (dir.v3-file* dir))
   (foldr + 0 (map (lambda (x) (how-many.v3 x)) (dir.v3-dir* dir)))))



;; Exercise 337















