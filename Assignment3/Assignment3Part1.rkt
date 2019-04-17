;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Assignment3Part1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget(name quantity time price parts))
;; a widget is a (make-widget String Natural Natural Number ListOfWidget)

;Templates


(define (fn-for-w w)
  (... (widget-name w)
       (widget-quantity w)
       (widget-time w)
       (widget-price w)
       (fn-for-low(widget-parts w))))

(define (fn-for-low low)
  (cond
    [(empty? low)...]
    [else
     (...(fn-for-w (first low))
     (fn-for-low (rest low)))])) 
    
;=========================================
(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 5 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 5 empty))
(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
(define Receiver (make-widget "Receiver" 10 5 7 empty))
(define Telephone (make-widget "Telephone" 5 20 15 (list Receiver Buttons Cord)))

(define Glass (make-widget "Glass" 6 9 4 empty))
(define Beads (make-widget "Beads" 25 12 7 (list Glass)))
(define Bracelet (make-widget "Bracelet" 5 3 5 (list Beads)))
(define Chain (make-widget "Chain" 7 2 1 empty))
(define Pendant (make-widget "Pendant" 4 3 1 empty))
(define Necklace (make-widget "Necklace" 10 7 3 (list Chain Pendant)))
(define Rings (make-widget "Rings" 15 8 11 empty))
(define Jewelry (make-widget "Jewelry set" 4 17 30 (list Rings Necklace Bracelet)))

(list Wire Cord Numbers Buttons Receiver Telephone Glass Beads Bracelet Chain Pendant Necklace Rings Jewelry empty)

;widget natural -> list of widget
;Return all of the widgets and subwidgets whose name length is longer than the given natural
;(define (find-widget-name-longer-than n x)empty) ;stub

(define (find-widget-name-longer-than w x)
  (if (> (string-length (widget-name w)) x)
       (cons w (find-widget-name-longer-than-low (widget-parts w) x))
       (find-widget-name-longer-than-low (widget-parts w) x)))

(define (find-widget-name-longer-than-low low x)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-name-longer-than(first low) x)
     (find-widget-name-longer-than-low (rest low) x))]))

;widget natural -> list of widget
;Return the widgets and subwidgets whose stock is greater than the given natural
;(define (find-widget-quantity-over w x) empty) ;stub

(define (find-widget-quantity-over w x)
  (if (> (widget-quantity w) x)
      (cons w (find-widget-quantity-over-low (widget-parts w) x))
      (find-widget-quantity-over-low(widget-parts w) x)))

(define (find-widget-quantity-over-low low x)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-quantity-over(first low)x)
     (find-widget-quantity-over-low (rest low) x))]))

;widget number -> list of widget
;Return the widgets and subwidgets whose price is less than the given number
;(define (find-widget-cheaper than w x)empty) stub
(define (find-widget-cheaper-than w x)
  (if (< (widget-price w) x)
      (cons w (find-widget-cheaper-low (widget-parts w) x))
      (find-widget-cheaper-low (widget-parts w) x)))

(define (find-widget-cheaper-low low x)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-cheaper-than (first low) x)
     (find-widget-cheaper-low (rest low) x))]))

;widget natural natural -> list of widget
;Return all widgets and subwidgets whose stock is not enough to make the given natural of items
;or whose price is greater than the second given natural
;(define (find-widget-hard-make w q p)empty) stub
(define (find-widget-hard-make-than w q p)
  (if (or (< (widget-quantity w) q) (> (widget-price w) p))
      (cons w (find-widget-hard-make-low (widget-parts w) q p))
      (find-widget-hard-make-low (widget-parts w) q p)))

(define (find-widget-hard-make-low low q p)
  (cond
    [(empty? low) empty]
    [else
     (append (find-widget-hard-make-than (first low) q p)
     (find-widget-hard-make-low (rest low) q p))]))

;================================================================
;Check Expects

(check-expect (find-widget-name-longer-than Beads 0)(list Beads Glass))
(check-expect (find-widget-name-longer-than Jewelry 50) empty)
(check-expect (find-widget-name-longer-than Rings 4)(list Rings))
(check-expect (find-widget-quantity-over Beads 10) (list Beads))
(check-expect (find-widget-quantity-over Wire 5)empty)
(check-expect (find-widget-quantity-over Cord 2)(list Cord Wire))
(check-expect (find-widget-quantity-over Receiver 9)(list Receiver))
(check-expect (find-widget-cheaper-than Telephone 2)empty)
(check-expect (find-widget-cheaper-than Pendant 7)(list Pendant))
(check-expect (find-widget-cheaper-than Rings 8)empty)
(check-expect (find-widget-cheaper-than Beads 10) (list Beads Glass))
(check-expect (find-widget-cheaper-than Beads 6) (list Glass))
(check-expect (find-widget-cheaper-than Beads 3) empty)
(check-expect (find-widget-hard-make-than Beads 2 1)(list Beads Glass))
(check-expect (find-widget-hard-make-than Beads 10 100) (list Glass))
(check-expect (find-widget-hard-make-than Beads 2 6) (list Beads))
(check-expect (find-widget-hard-make-than Jewelry 0 0)(list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect (find-widget-hard-make-than Rings 100 100)(list Rings))

;====================================================================




  