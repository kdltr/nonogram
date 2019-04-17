#!/bin/sh
#|
exec csi -s $0 $@
|#

;; Usage: nonogram [WIDTH HEIGHT]

;; Controls (qwerty):
;; - arrow keys: move around
;; - f: mark square as lit
;; - d: mark square as not lit
;; - a: reset square
;; - q or return: quit and check answer

;; You need an UTF-8 enabled terminal
;; and a font that provides these characters:
;; - ■
;; - □
;; - ☒

;; Zoom support is recommended
;; The checker doesn’t work when there are multiple solutions

(import
  scheme
  (chicken base)
  (chicken process-context)
  (chicken random)
  (srfi 1)
  ansi-escape-sequences
  stty
  utf8
  vector-lib)

(set-pseudo-random-seed! (random-bytes))

(define-record grid width height data)

(define (grid-ref g x y)
  (vector-ref (grid-data g)
              (+ x (* y (grid-width g)))))

(define (grid-set! g x y val)
  (vector-set! (grid-data g)
               (+ x (* y (grid-width g)))
               val))

(define (empty-grid width height)
  (make-grid width height
             (make-vector (* width height) #f)))

(define (random-grid width height)
  (make-grid width height
             (random-vector (* width height))))

(define (random-vector size)
  (vector-unfold (lambda (_) (zero? (pseudo-random-integer 2)))
                 size))

(define (compute-hints limit ref)
  (remove!
    zero?
    (let lp ((i 0) (prev 0))
      (cond ((= i limit) (list prev))
            ((eqv? (ref i) #t)
             (lp (add1 i) (add1 prev)))
            (else (cons prev (lp (add1 i) 0)))))))

(define (compute-hints/lines g)
  (let lp ((j 0))
    (if (= j (grid-height g))
        '()
        (cons (compute-hints (grid-width g) (lambda (i) (grid-ref g i j)))
              (lp (add1 j))))))

(define (compute-hints/columns g)
  (let lp ((i 0))
    (if (= i (grid-width g))
        '()
        (cons (compute-hints (grid-height g) (lambda (j) (grid-ref g i j)))
              (lp (add1 i))))))

(define (biggest-list l)
  (fold (lambda (l m) (max m (length l))) 0 l))

(define (display-grid g line-hints column-hints)
  (for-each display-hints column-hints)
  (let lpi ((i 0)
            (line-hints line-hints))
    (unless (= i (grid-height g))
      (let lpj ((j 0))
        (unless (= j (grid-width g))
          (display (case (grid-ref g j i)
                     ((#t) #\■)
                     ((#f) #\□)
                     ((x)  #\☒)))
          (display #\space)
          (lpj (add1 j))))
      (display-hints (car line-hints))
      (lpi (add1 i)
           (cdr line-hints)))))


(define (display-hints hs)
  (for-each
    (lambda (h) (display (if (zero? h) #\space h)) (display #\space))
    hs)
  (newline))

(define (normalize-hints hs m)
  (map (lambda (h)
         (append (make-list (- m (length h)) 0) h))
    hs))

(define (rotate-list l inner-len)
  (let lp ((i 0))
    (if (= i inner-len)
        '()
        (cons (map (lambda (il) (list-ref il i)) l)
              (lp (add1 i))))))

(define (clamp x low hi)
  (min (max x low) hi))

(define width 10)
(define height 10)

(when (= (length (command-line-arguments)) 2)
  (set! width (string->number (car (command-line-arguments))))
  (set! height (string->number (cadr (command-line-arguments)))))

(define g (random-grid width height))
(define line-hints (compute-hints/lines g))
(define col-hints (compute-hints/columns g))
(define max-line (biggest-list line-hints))
(define max-col (biggest-list col-hints))
(define display-column-hints (rotate-list (normalize-hints col-hints max-col)
                                          max-col))

(define play-grid (empty-grid (grid-width g) (grid-height g)))

(define (input)
  (let ((char (read-char)))
    (or (and (or (eqv? char #\e)
                 (eqv? char #\f))
             'enable)
        (and (or (eqv? char #\i)
                 (eqv? char #\d))
             'disable)
        (and (eqv? char #\a)
             'reset)
        (and (or (eqv? char #\q)
                 (eqv? char #\return))
             'quit)
        (and (eqv? char #\escape)
             (eqv? (read-char) #\[)
             (case (read-char)
               ((#\A) 'up)
               ((#\B) 'down)
               ((#\C) 'right)
               ((#\D) 'left)
               (else #f)))
        )))

(define x 0)
(define y 0)

(with-stty '((not icanon) (not echo))
  (lambda ()
    (display "\x1b[?1049h")
    (define *exit* #f)
    (let lp ()
      (display (cursor-position 1 1))
      (display (erase-display))
      (display-grid play-grid line-hints display-column-hints)
      (display (cursor-position (+ y 1 max-col) (+ (* x 2) 1)))
      (case (input)
        ((up) (set! y (clamp (- y 1) 0 (- height 1))))
        ((down) (set! y (clamp (+ y 1) 0 (- height 1))))
        ((right) (set! x (clamp (+ x 1) 0 (- width 1))))
        ((left) (set! x (clamp (- x 1) 0 (- width 1))))
        ((enable) (grid-set! play-grid x y #t))
        ((disable) (grid-set! play-grid x y 'x))
        ((reset) (grid-set! play-grid x y #f))
        ((quit) (set! *exit* #t)))
      (unless *exit* (lp)))))

(display "\x1b[?1049l")

(if (and (equal? (compute-hints/lines play-grid)
                 line-hints)
         (equal? (compute-hints/columns play-grid)
                 col-hints))
    (print "Nice!")
    (print "Nope :("))
(display-grid play-grid line-hints display-column-hints)
