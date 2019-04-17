;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)

(define SIZE 900) ;; can be between 300 and 900
(define STARTING-DEPTH 4) ;number of turns the computer uses in its minimax algorithm 

;; can use this to draw the board
(define PEN (make-pen "LightSalmon" 15 "solid" "round" "round"))
(define MTS (empty-scene SIZE SIZE))
(define TEXT-SIZE (quotient SIZE 6))

(define X (text "X" TEXT-SIZE "red"))
(define O (text "O" TEXT-SIZE "blue"))
(define SPACING (quotient SIZE 8))
(define PLAYING 5)

;; "strongly suggest" constructing a word state that has (at minimum):
;;      board 
;;      state of the board
;;      whose turn it is

;; ws is (make-ws listOfPlayer (make-player Natural|false image) Natural)
;; interp   - board is a list of players which shows board with players on it
;;         - player is the player that is taking turn (either X, O or B (blank))
;;         - turns is the depth that computer need to look ahead fro its move           
(define-struct ws (board player turns))

;; player is a (make-player Natural|false image)
(define-struct player (num img))
(define B (make-player false (square 0 "solid" "white")))
(define P1 (make-player 1 X))
(define P2 (make-player 2 O))

(define ROW (list (list 0 1 2)
                  (list 3 4 5)
                  (list 6 7 8)))
(define COL (list (list 0 3 6)
                  (list 1 4 7)
                  (list 2 5 8)))
(define CROSS (list (list 0 4 8)
                    (list 2 4 6)))

(define UNITS (append ROW COL CROSS))

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

(define BD5
  (list P2 P1 P1
        P1 P2 P1
        B P1 P2))

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

;; Move is a (make-move ListOfPlayers Integer)
;; interpret as a possible move with a board and a win/loss value
(define-struct move (board value))


;; Board Player -> (listof Board)
;; creates all the possible moves a given player could make on a given board
(check-expect (successors BD1 P2) (build-list 9 (λ (x) (fill-square BD1 x P2))))

(define (successors board player) (remf* (λ (state) (equal? state board)) (build-list 9 (λ (x) (fill-square board x player)))))

;;board -> boolean|list
;; checks if the board is empty, if it is return false, else return a list of players
(check-expect (check-empty BD1) true)
(check-expect (check-empty BD4) false)
(check-expect (check-empty BD5) true)

(define (check-empty board)
  (member B board)) 

;;board player -> boolean
;;checks if the given player win the game, if so return true, else return false
(check-expect (win? BD1 P1) false)
(check-expect (win? BD2 P1) false)
(check-expect (win? BD3 P1) true)
(check-expect (win? BD3 P2) false)
(check-expect (win? BD4 P1) false)
(check-expect (win? BD5 P2) true)
(check-expect (win? BD5 P1) false)

(define (win? board player)
  (ormap (λ (unit)
           (andmap (λ (pos)
                     (equal? player (list-ref board pos)))
                   unit))
         UNITS))

;; Board -> Number
;; determines if the game is finished, and if so returns the natural of the player that won
;; Returns one of:
;;       - PLAYING (game is not completed)
;;       - 1  (player 2, the computer, wins)
;;       - -1  (player 1, the player, wins)
;;       - 0  (draw)
(check-expect (board-state BD1) PLAYING)
(check-expect (board-state BD2) PLAYING)
(check-expect (board-state BD3) -1)
(check-expect (board-state BD4) 0)
(check-expect (board-state BD5) 1)

(define (board-state board)
  (cond [(win? board P1) -1]
        [(win? board P2) 1]
        [else (if (check-empty board)
                  PLAYING
                  0)]))

;; Board Natural -> Board
;; makes the next move of the computer player based on the given board
;; computer will look ahead number of turns ahead to choose the best move
;; returns a changed board

(check-expect (next-move (list P1 B B
                               B B B
                               B B B) 9) (list P1 B B
                                               B P2 B
                                               B B B))
(check-expect (next-move (list P1 P1 B
                               B P2 B
                               B B B)  7) (list P1 P1 P2
                                             B P2 B
                                             B B B))
(define (next-move board turns)
  (local [(define (min-max state turns)
            (local [(define moves (map (λ (b) (make-move b (min-value b turns))) (successors state P2)))
                    (define value (move-value (argmax move-value moves)))
                    (define max-moves (remf* (λ (move) (> value (move-value move))) moves))]
              (move-board (list-ref max-moves (random (length max-moves)))))
            )
          (define (max-value state turns)
            (local [(define next (successors state P2))]
              (cond [(<= turns 0) (if (= (board-state state) PLAYING)
                                      0
                                      (board-state state))]
                    [(member 1 (map board-state next)) 1]
                    [(= (board-state state) PLAYING)
                     (apply max (map (λ (state) (min-value state (sub1 turns))) next))]
                    [else (board-state state)])))
          (define (min-value state turns)
            (local [(define next (successors state P2))]
              (cond [(<= turns 0) (if (= (board-state state) PLAYING)
                                      0
                                      (board-state state))]
                    [(member -1 (map board-state next)) -1]
                    [(= (board-state state) PLAYING)
                     (apply min (map (λ (state) (max-value state (sub1 turns))) (successors state P1)))]
                    [else (board-state state)])))
          ]
    (min-max board turns)))

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
;; make world state with a move made by the computer and switches the player to P1
;; switches the player to B if the game has ended
(define (tock ws)
  (cond [(> PLAYING (board-state (ws-board ws))) (make-ws (ws-board ws) B (ws-turns ws))]
        [(equal? P2 (ws-player ws))
         (make-ws (next-move (ws-board ws) (ws-turns ws)) P1 (ws-turns ws))]
        [else ws]))

;; WS -> Image
;; draws the world state, draws the board with lines and the X and Os
(define (render ws)
  (if (equal? B (ws-player ws))
      (place-image/align (cond [(= -1 (board-state (ws-board ws)))  (text "Player 1 Wins" (quotient TEXT-SIZE 2) "orange")]
                               [(= 0 (board-state (ws-board ws))) (text "Draw" (quotient TEXT-SIZE 2) "green")]
                               [(= 1 (board-state (ws-board ws))) (text "Computer Wins"  (quotient TEXT-SIZE 2) "purple")])
                         (/ SIZE 2) (/ SIZE 2) "middle" "middle"
                         (draw-values (ws-board ws) (draw-board SPACING)))
      (draw-values (ws-board ws) (draw-board SPACING))))

;; WS Integer Integer Mouseevent -> WS
;; returns a new world state with a changed board based on the input
;; switches the player to P2
(define (handle-click ws x y me)
  (if (and (equal? P1 (ws-player ws))
           (string=? me "button-down")
           (>= (r-c->posn (quotient (- y SPACING) (* 2 SPACING))
                          (modulo (quotient (- x SPACING) (* 2 SPACING))(* 2 SPACING))) 0))
      (make-ws (fill-square (ws-board ws) (r-c->posn (quotient (- y SPACING) (* 2 SPACING))
                                                     (modulo (quotient (- x SPACING) (* 2 SPACING))(* 2 SPACING))) P1) P2 (ws-turns ws))
      ws))

;; will need this for extra credit
;; WS KeyEvent -> WS
;; change turn (ws-turns ws) to a keyevent (only work for number)
(define (handle-key ws ke)
  (if (false? (string->number ke))
      ws
      (make-ws (ws-board ws) (ws-player ws) (string->number ke))))

(define (main ws)
  (big-bang ws ; WS
    (on-tick tock) ; WS -&gt; WS
    (to-draw render) ; WS -&gt; Image
    (on-mouse handle-click) ; WS Integer Integer MouseEvent -&gt; WS
    (on-key handle-key))) ; WS KeyEvent -&gt; WS

(define START (make-ws BD1 P1 STARTING-DEPTH))
(main START)