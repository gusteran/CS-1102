;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname GameOfLife) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define ALIVE 1)
(define DEAD 0)
(define W 800)
(define H 600)
(define MTS (empty-scene W H))

;; Natural -> Row
;; creates a random row based on the width
(define (row width)
  (if (> width 0)
      (cons (random 2) (row (- width 1)))
      empty))

;; Natural Natural -> Board
;; generates a random board based on the width and height
(define (random-board width height)
  (if (> height 0)
      (cons (row width) (random-board width (- height 1)))
      empty))

;; Board Natural Natural -> Natural
;; returns the value at a location on the board, returns dead if off the board
(define (value board x y)
  (if (or (< x 0) (< y 0) (>= x (length (list-ref board 0))) (>= y (length board)))
      DEAD
      (list-ref (list-ref board y) x)))
      
;; Board Natural Natural -> Natural
;; returns the amount of living neighbors to a cell
(define (count-neighbors board x y)
  (+ (value board (- x 1) (+ y 1)) (value board x (+ y 1)) (value board (+ x 1) (+ y 1))
     (value board (- x 1) y)                               (value board (+ x 1) y)
     (value board (- x 1) (- y 1)) (value board x (- y 1)) (value board (+ x 1) (- y 1))))

;; Board Natural Natural -> Boolean
;; returns whether a cell will live based on how many living neighbors it has
(define (live? board x y)
  (if (= (value board x y) ALIVE)
      (or (= (count-neighbors board x y) 2) (= (count-neighbors board x y) 3))
      (= (count-neighbors board x y) 3)))

;; Board Row -> Row
;; sets the next generation for the row
;; set x to 0
(define (next-generation-row board x y)
  (if (< x (length (list-ref board y)))
      (cons (if (live? board x y) ALIVE DEAD) (next-generation-row board (+ x 1) y))
      empty))

;; Board -> Board
;; Returns the next generation of the board
;; Set x and y to 0 when calling the function
(define (next-generation board x y)
  (if (< y (length board))
      (cons (next-generation-row board x y) (next-generation board x (+ y 1)))
      empty))

;; board -> board
;; start the world with a board
(define (main board)
  (big-bang board                   ; board
    (on-tick break)
            (to-draw   render)))   ; board -> Image
            ;(on-key  advance)))      ; board Integer Integer MouseEvent -> board

;; board Natural Natural -> Image
;; helper for render for each row
(define (render-row board x y)
  (if (< x (length (list-ref board y)))
      (beside (rectangle (/ W (length (list-ref board y))) (/ H (length board))
                        (if(= (value board x y) ALIVE) "solid" "outline") "black")
              (render-row board (+ x 1) y))
      (rectangle 0 0 "solid" "black")))

;; board Natural Natural -> Image
;; helper for render for the board
(define (render-board board x y)
  (if(< y (length board))
       (above (render-row board x y) (render-board board 0 (+ y 1)))
       (rectangle 0 0 "solid" "black")))

;; board -> Image
;; render ...
(define (render board) (place-image (render-board board 0 0) (/ W 2) (/ H 2) MTS))

;; board Integer Integer MouseEvent -> board
(define (advance board ke)
  (if (key=? ke " ")
      (next-generation board 0 0)
      board))

;; WARNING: WILL MELT CPU
(define (break board) (next-generation board 0 0))

;; testing
;(define a (random-board 5 5))
;a
(main (random-board 50 50))