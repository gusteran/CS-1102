;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment3Part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct widget(name quantity time price parts))
;; a widget is a (make-widget String Natural Natural Number ListOfWidget)

;Template

(define (fn w)
  (local [
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
                   (fn-for-low (rest low)))])) ]
    (fn-for-w w)))
    
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

;==============================================================

(define (yes x) (> x 0))

;; widget function -> list of widget
;; returns a list of the widget and subwidgets that yield true to the function given the widget
(define (filter-widget w fn)
  (local [
          (define (fn-for-w w)
            (if (fn w)
                (cons w (fn-for-low(widget-parts w)))
                (fn-for-low(widget-parts w))))

          (define (fn-for-low low)
            (cond
              [(empty? low) empty]
              [else
               (append (fn-for-w (first low))
                       (fn-for-low (rest low)))])) ]
    (fn-for-w w)))

;widget natural -> list of widget
;Return all of the widgets and subwidgets whose name length is longer than the given natural
;(define (find-widget-name-longer-than w x)
;  (filter-widget w (lambda (x) (> (string-length (widget-name w)) x))))

(define (find-widget-name-longer-than w x)
  (filter-widget w (lambda (w) (> (string-length (widget-name w)) x))))


;widget natural -> list of widget
;Return the widgets and subwidgets whose stock is greater than the given natural
(define (find-widget-quantity-over w x)
  (filter-widget w (lambda (w) (> (widget-quantity w) x))))


;widget number -> list of widget
;Return the widgets and subwidgets whose price is less than the given number
(define (find-widget-cheaper-than w x)
  (filter-widget w (lambda (w) (< (widget-price w) x))))


;widget natural natural -> list of widget
;Return all widgets and subwidgets whose stock is not enough to make the given natural of items
;or whose price is greater than the second given natural
(define (find-widget-hard-make-than w q p)
  (filter-widget w (lambda (w) (or (< (widget-quantity w) q) (> (widget-price w) p)))))

;widget -> (listof widget)
;return the given widget and all of its subwidgets
(define (gather-widgets w)
  (filter-widget w (lambda (w) (not(empty? w)))))

;widget fn -> (listof widget)
;sorts the widgets
(define (sort-widgets w fn)
  (local [(define (sort-LOW LOW fn)
            (cond [(< (length LOW) 2) LOW]
                  [else
                   (append
                    (sort-LOW (filter (lambda (w) (fn w (first LOW))) (rest LOW)) fn)
                    (list (first LOW))
                    (sort-LOW (filter (lambda (w) (not (fn w (first LOW)))) (rest LOW)) fn))]))
          ]
    (sort-LOW (gather-widgets w) fn)))

;widget -> (listof widget)
;example of a use for sort-widgets that sorts in order of number of parts
(define (sort-widgets-quantity w)
  (sort-widgets w (lambda (w1 w2) (> (widget-quantity w1) (widget-quantity w2)))))

;====================================================
;; Check Expects

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
(check-expect (gather-widgets Beads) (list Beads Glass))
(check-expect (gather-widgets Jewelry)(list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect (sort-widgets-quantity Beads) (list Beads Glass))
(check-expect (sort-widgets-quantity Jewelry) (list Beads Rings Necklace Chain Glass Bracelet Jewelry Pendant))
