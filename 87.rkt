;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |87|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)

(define-struct editor [string index])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is string with 
; the cursor displayed in position index

(define HEIGHT 20)
(define WIDTH 200)
(define BCKG (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 20 "solid" "red"))
(define FONT-SIZE 16)
(define MAX-NUM-OF-STRINGS (quotient WIDTH FONT-SIZE))

; String, Number -> String
; returns rest of the string s starting from n
(check-expect (string-rest "world" 1) "orld")
(check-expect (string-rest "world" 4) "d")
(check-expect (string-rest "world" 5) "")
(define (string-rest s n)
  (if (and (>= n 0) (< n (string-length s))) (substring s n) ""))

; String, Number -> String
; returns substring from string s (up to n)
(check-expect (string-up-to "world" 1) "w")
(check-expect (string-up-to "world" 4) "worl")
(check-expect (string-up-to "world" 5) "world")
(check-expect (string-up-to "world" 10) "world")

(define (string-up-to s n)
  (if (and (>= n 0)(< n (string-length s))) (substring s 0 n) s))

; String -> Image
; returns text from provided string s
(check-expect (make-text "hello world") (text "hello world" FONT-SIZE "black"))
(define (make-text s) (text s 16 "black"))

; String, String, Number -> String
; returns a string s with insert-s inserted string in insert-pos position                                                  
(check-expect (string-insert "Hello world" "w" 0) "wHello world")
(check-expect (string-insert "Hello world" "s" 5) "Hellos world")
(check-expect (string-insert "Hello world" "w" 11) "Hello worldw")

(define (string-insert s insert-s insert-pos)
  (string-append (substring s 0 insert-pos) insert-s (substring s insert-pos)))

; String -> String
; returns string without character on pos
(check-expect (string-remove-pos "loh" 0) "oh")
(check-expect (string-remove-pos "loh" 1) "lh")
(check-expect (string-remove-pos "loh" 2) "lo")
(check-expect (string-remove-pos "loh" -1) "")
(define (string-remove-pos s pos)
  (if (>= pos 0)
      (string-append (substring s 0 pos) (substring s (+ pos 1) (string-length s)))
  ""))

; String, Number -> Boolean
; checks if n index is in s string
(check-expect (in-string? "world" -1) false)
(check-expect (in-string? "world" 0) true)
(check-expect (in-string? "world" 5) false)

(define (in-string? s n)
  (if (and (>= n 0) (< n (string-length s))) true false))

; Editor -> Image
; renders text within an empty scene;
(define (render ed)
  (overlay/align "left" "center"
                 (beside (make-text (string-up-to (editor-string ed) (editor-index ed)))
                         CURSOR
                         (make-text (string-rest (editor-string ed) (editor-index ed))))
                 BCKG))

; Editor, KeyEvent -> Editor
; Its task is to add a single-character KeyEvent ke to the end of the pre field of ed,
; unless ke denotes the backspace ("\b") key.
; In that case, it deletes the character immediately to the left of the cursor(if there are any).
; The function ignores the tab key ("\t") and the return key ("\r").

(check-expect (edit (make-editor "Hello World" 0) "left")(make-editor "Hello World" 0))
(check-expect (edit (make-editor "Hello World" 1) "left")(make-editor "Hello World" 0))
(check-expect (edit (make-editor "Hello World" 20) "left")(make-editor "Hello World" 10))

(check-expect (edit (make-editor "Hello World" 0) "right")(make-editor "Hello World" 1))
(check-expect (edit (make-editor "Hello World" 5) "right")(make-editor "Hello World" 6))
(check-expect (edit (make-editor "Hello World" 11) "right")(make-editor "Hello World" 11))

(define (edit ed ky)
  (cond
    [(or (key=? ky "\t") (key=? ky "\r")) ed]
    [(key=? ky "\b")(make-editor (string-remove-pos (editor-string ed) (- (editor-index ed) 1))
                                 (if (> (editor-index ed) 0) (- (editor-index ed) 1) (editor-index ed)))]
    [(key=? ky "left") (if (in-string? (editor-string ed) (editor-index ed))
                           (make-editor (editor-string ed) (if (> (editor-index ed) 0) (- (editor-index ed) 1) 0))
                           (make-editor (editor-string ed) (if (> (editor-index ed) (string-length (editor-string ed))) (- (string-length (editor-string ed)) 1) (- (editor-index ed) 1))))]
    [(key=? ky "right") (if (in-string? (editor-string ed) (editor-index ed))
                            (make-editor (editor-string ed) (if (>= (editor-index ed) 0) (+ (editor-index ed) 1) 0))
                            (make-editor (editor-string ed) (string-length (editor-string ed))))]
    [else (make-editor (string-insert (editor-string ed) ky (editor-index ed)) (+ (editor-index ed) (string-length ky)))]))

; Editor -> Editor
(define (run s)
  (big-bang s
    [to-draw render]
    [on-key edit]))

(run (make-editor "Hello World" 11))