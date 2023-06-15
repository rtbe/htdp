;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 338-344) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/abstraction)
(require htdp/dir)

; Dir.v3 -> N
; Counts files withing a directory tree

(define dt.6 (make-dir "TS" (list (make-dir "Text"  '() (list (make-file "part1" 99 "")
                                                                    (make-file "part2" 52 "")
                                                                    (make-file "part3" 17 "")))
                                     (make-dir "Libs" (list (make-dir"Code" '() (list (make-file "hang" 8 "")
                                                                                             (make-file "draw" 2 "")))
                                                               (make-dir "Docs" '() (list (make-file "read!" 19 "")))) '()))
                          (list (make-file "read!" 10 ""))))

(check-expect (how-many dt.6) 7)

(define (how-many dir)
  (+ (length (dir-files dir))
     (for/sum ([i (dir-dirs dir)]) (how-many i))))

(define L (create-dir "/var/log/"))

;; Exercise 338

(how-many L)

; I am confident in behaviour of how-many function only because of passed tests

;; Exercise 339

; Dir String -> Boolean
; consumes a Dir and a file name and determines whether or not a file with this name
; occurs in the directory tree

(check-expect (find? L "loh") #f)
(check-expect (find? L "wifi.log") #t)
(check-expect (find? L "2022.11.03.asl") #t)

(define (find? dir name)
  (or (ormap (lambda (file) (string=? (file-name file) name)) (dir-files dir))
      (ormap (lambda (subdir) (find? subdir name)) (dir-dirs dir))))
  

;; Exercise 340

; Dir -> [List-of String]
; lists the names of all files and directories in a given Dir
(define (ls dir)
  (append (list (dir-name dir))
          (append (for/list ([file (dir-files dir)]) (file-name file))
                  (for/list ([subdir (dir-dirs dir)]) (ls subdir)))))

(ls L)

;; Exercise 341

; consumes a Dir and computes the total size of all
; the files in the entire directory tree

(define (du dir)
  (+ (for/sum ([f (dir-files dir)]) (file-size f))
      (for/sum ([d (dir-dirs dir)]) (+ 1 (du d)))))

(du L)