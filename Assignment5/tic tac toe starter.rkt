;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tic tac toe starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)

(define SIZE 800) ;; can be between 300 and 900

;; can use this to draw the board
(define PEN (make-pen "LightSalmon" 15 "solid" "round" "round"))
(define MTS (empty-scene SIZE SIZE))
(define TEXT-SIZE (quotient SIZE 6))

(define X (text "X" TEXT-SIZE "red"))
(define O (text "O" TEXT-SIZE "blue"))
(define SPACING (/ SIZE 8))

;; "strongly suggest" constructing a word state that has (at minimum):
;;      state of the board
;;      whose turn it is
(define-struct ws (board player))

(define-struct player (num img))
(define B (make-player false (square 0 "solid" "white")))
(define P1 (make-player 1 X))
(define P2 (make-player 2 O))

;; POS is Natural[0, 8]
;; interpret the position of a square on the board for a given p
;;         - the row is    (quotient p 3)
;;         - the column is (remainder p 3)

;; Convert row and column to posn
;; returns -1 if is not [0,2] and [0,2]
(define (r-c->posn r c)
  (if (and (>= r 0) (< r 3) (>= c 0) (< c 3)) 
      (+ (* r 3) c)
      -1))

;; example boards
(define positions
  (list 0 1 2
        3 4 5
        6 7 8))

(define BD1
  (list B B B
        B B B
        B B B))

(define BD2
  (list B P1 B
        B P2 B
        B B B))

(define BD3
  (list P1 B B
        P1 B B
        P1 B B))

(define BD4
  (list P1 P2 P1
        P1 P2 P2
        P2 P1 P1))

;; one easy way to draw a line
;(add-line MTS 50 50 250 50 PEN)

;; Board Pos -> Player | False
;; returns the player at the given position
(check-expect (read-square BD1 (r-c->posn 0 0)) B)
(check-expect (read-square BD4 (r-c->posn 1 1)) P2)
(check-expect (read-square BD3 (r-c->posn 0 0)) P1)
(check-expect (read-square BD1 (r-c->posn 4 4)) false)

(define (read-square board posn)
  (if (>= posn 0)
      (list-ref board posn)
      false))

;; Board Pos Player -> Board
;; returns a board with the player at the given position
(check-expect (fill-square BD1 (r-c->posn 0 0) P1) (cons P1 (rest BD1)))
(check-expect (fill-square BD4 (r-c->posn 0 0) P1) BD4)
(check-expect (fill-square BD1 (r-c->posn 4 4) P1) BD1)

(define (fill-square board posn player)
  (if (and (>= posn 0) (equal? (read-square board posn) B))
      (append (take board posn)
              (list player)
              (drop board (add1 posn)))
      board))


;; Board -> Board
;; makes the next move of the computer player
;; !!!
#;(define (next-move board)
  (local
    [(define (terminal? board) (< (board-state 4)))
     (define (min-max board)
       (local [(define possible-moves (
     (define (max-value board)
       (if (terminal? board)
           (board-state board)
           (apply min-value (successors board P2))))
     (define (min-value board) ...)
     (define (successors board player) ...)
     (define lom (max-value board))
    (fill-square (min-max board)))(fill-square board (min-max board 2)))]))]))
;; Board -> Number
;; determines if the game is finished, and if so returns the natural of the player that won
;; Returns one of:
;;       - 5 (game is not completed)
;;       - 1  (player 1 wins)
;;       - -1  (player 2 wins)
;;       - 0  (draw)
;; !!!
(define (board-state board) -1)

;; Natural -> Image
;; creates the board as an image
(define (draw-board SPACING)
  (add-line (add-line (add-line (add-line MTS (* 3 SPACING) SPACING (* 3 SPACING) (* 7 SPACING) PEN)
                                                  (* 5 SPACING) SPACING (* 5 SPACING) (* 7 SPACING) PEN)
                                        SPACING (* 3 SPACING) (* 7 SPACING) (* 3 SPACING) PEN)
                              SPACING (* 5 SPACING) (* 7 SPACING) (* 5 SPACING) PEN))

;; Board -> Image
;; draws the X and Os as an image
(define (draw-values board scene)
  (local [(define (draw board loc)
            (cond [(empty? board) scene]
                  [else
                   (place-image/align (player-img (first board))
                                      (* 2 (+ (* (modulo loc 3) SPACING) SPACING)) (* 2 (+ (* (quotient loc 3) SPACING) SPACING)) "middle" "middle"
                                      (draw (rest board) (add1 loc)))]))]
    (draw board 0)))


;; WS-> WS
;; !!!
(define (tock ws)
  (if (equal? P2 (ws-player ws))
      (make-ws (next-move (ws-board ws)) P2)
      ws))

;; WS -> Image
;; draws the world state, draws the board with lines and the X and Os
(define (render ws)
  (draw-values (ws-board ws) (draw-board SPACING)))

;; WS Integer Integer Mouseevent -> WS
(check-expect (handle-click (make-ws BD1 P1) (* 2 SPACING) (* 2 SPACING) "button-down") (make-ws (fill-square (ws-board (make-ws BD1 P1)) 0 P1) P2))


(define (handle-click ws x y me)
  (if (and (equal? P1 (ws-player ws)) (string=? me "button-down"))
      (make-ws (fill-square (ws-board ws) (r-c->posn (quotient (- y SPACING) (* 2 SPACING))
                                        (modulo (quotient (- x SPACING) (* 2 SPACING))(* 2 SPACING))) P1) P2)
      ws))

;; will need this for extra credit
;;!!!
(define (handle-key ws ke) ws)

(define (main ws)
  (big-bang ws ; WS
    (on-tick tock) ; WS -&gt; WS
    (to-draw render) ; WS -&gt; Image
    (on-mouse handle-click) ; WS Integer Integer MouseEvent -&gt; WS
    (on-key handle-key))) ; WS KeyEvent -&gt; WS

(main (make-ws BD1 P1))