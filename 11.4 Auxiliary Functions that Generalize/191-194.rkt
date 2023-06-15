;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 191-194) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define triangle-p
  (list
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 30 20)))
 	
(define square-p
  (list
    (make-posn 10 10)
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 10 20)))

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)

; a plain background image 
(define MT (empty-scene 50 50))

; Image Posn Posn -> Image 
; renders a line from p to q into img
(define (render-line img p q)
  (scene+line
    img
    (posn-x p) (posn-y p) (posn-x q) (posn-y q)
    "red"))
 
; Image Polygon -> Image
; renders the given polygon p into img

(check-expect
  (render-poly MT triangle-p)
  (scene+line
    (scene+line
      (scene+line MT 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red"))

(check-expect
  (render-poly MT square-p)
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))

(define (render-poly img p)
  (cond
    [(empty? (rest (rest (rest p))))
     (render-line
       (render-line
         (render-line MT (first p) (second p))
         (second p) (third p))
       (third p) (first p))]
    [else
     (render-line (render-poly img (rest p))
                  (first p)
                  (second p))]))

; An NELoP is one of: 
; – (cons Posn '())
; – (cons Posn NELoP)


; Image NELoP -> Image 
; connects the dots in p by rendering lines in img

(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 20 30 20 "red")
               20 10 20 20 "red"))

(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else
     (render-line (connect-dots img (rest p))
                  (first p)
                  (second p))]))

;; Exercise 191

(check-expect
  (connect-dots MT square-p)
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red"))

;; Exercise 192

; last takes NELoP as first parameter which definition is larger than Polygon one
; So all Polygons are valid NELoP and we can use Poligon where NELoP is needed


; Image Polygon -> Image 
; adds a corner of p to img

(check-expect
  (render-poly.v2 MT triangle-p)
  (scene+line
    (scene+line
      (scene+line MT 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red"))

(check-expect
  (render-poly.v2 MT square-p)
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))

(define (render-poly.v2 img p)
  (render-line (connect-dots.v2 img p) (first p) (last p)))

; Image NELoP -> Image
; connects the Posns in p in an image
(define (connect-dots.v2 img p)
  (cond
    [(empty? (rest p)) img]
    [else (render-line (connect-dots.v2 img (rest p))
                       (first p)
                       (second p))]))

; Image Posn Posn -> Image 
; draws a red line from Posn p to Posn q into im
(define (render-line.v2 im p q)
  (scene+line
    im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
 
; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))


;; Exercise 193

(check-expect
  (render-poly.v4 MT triangle-p)
  (scene+line
    (scene+line
      (scene+line MT 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red"))

(check-expect
  (render-poly.v4 MT square-p)
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))

; Image Polygon -> Image 
; adds a corner of p to img
(define (render-poly.v4 img p)
  (connect-dots.v2 img (cons (last p) p)))

(check-expect
  (render-poly.v5 MT triangle-p)
  (scene+line
    (scene+line
      (scene+line MT 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red"))

(check-expect
  (render-poly.v5 MT square-p)
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))

; Image Polygon -> Image 
; adds a corner of p to img
(define (render-poly.v5 img p)
  (connect-dots.v2 img (add-at-end p (first p))))

; Polygon Posn -> Polygon
; adds posn at the end of p
(define (add-at-end p posn)
  (cond
    [(empty? (rest p)) (cons (first p) (cons posn '()))]
    [else (cons (first p) (add-at-end (rest p) posn))]))


;; Exercise 194

; Image NELoP Posn -> Image
; connects the Posns in p in an image
; and connects posn to that image
(define (connect-dots.v3 img p posn)
  (cond
    [(empty? (rest p)) (render-line.v2 img (first p) posn)]
    [else (render-line.v2 (connect-dots.v3 img (rest p) posn)
                          (first p)
                          (second p))]))

(check-expect
  (render-poly.v3 MT triangle-p)
  (scene+line
    (scene+line
      (scene+line MT 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red"))

(check-expect
  (render-poly.v3 MT square-p)
  (scene+line
    (scene+line
      (scene+line
        (scene+line MT 10 10 20 10 "red")
        20 10 20 20 "red")
      20 20 10 20 "red")
    10 20 10 10 "red"))

(define (render-poly.v3 img p)
  (connect-dots.v3 img p (first p)))