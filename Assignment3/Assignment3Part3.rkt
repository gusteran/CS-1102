;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment3Part3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define BLANK (square 0 "solid" "red"))
(define TEXT-SIZE 24)    
(define TEXT-COLOR "black")  ;; not sure if i should have this???  maybe good for testing???
(define TAB 5) ; someone senior told me this constant might help

;=========================================
(define-struct widget(name quantity time price parts))
;; a widget is a (make-widget String Natural Natural Number ListOfWidget)

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
;=========================================

;; trying it with ISL-lambda rather than BSL (just don't tell the boss)
;; Natural -> String
;; creates a blank string of length equal to n
(define (blanks n)
  (list->string (build-list n (lambda (x) #\ ))))

(check-expect (blanks 0) "")
(check-expect (blanks TAB) "     ")

;; widget -> image
;; renders the widget and its subwidgets in a heirarchy by returning an image
(define (simple-render w) (render w (lambda (w) TEXT-COLOR)))
#;(define (simple-render w)
    (local [
            (define (simple-render-w w str)
              (above/align "left" (text (string-append str (widget-name w) " : "
                                                       (number->string (widget-quantity w)) " @ $"
                                                       (number->string (widget-price w)))
                                        TEXT-SIZE TEXT-COLOR)
                           (simple-render-low(widget-parts w) (string-append (blanks TAB) str))))

            (define (simple-render-low low str)
              (cond
                [(empty? low) BLANK]
                [else
                 (above/align "left" (simple-render-w (first low) str)
                              (simple-render-low (rest low) str))]))]
      (simple-render-w w "")))

;; widget fn -> image
;; renders the widget and its subwidgets in a heirarchy by returning an image
;; with color for each text defined by the given function
(define (render w fn)
  (local [
          (define (render-w w str)
            (above/align "left" (text (string-append str (widget-name w) " : "
                                                     (number->string (widget-quantity w)) " @ $"
                                                     (number->string (widget-price w)))
                                      TEXT-SIZE (fn w))
                         (render-low(widget-parts w) (string-append (blanks TAB) str))))

          (define (render-low low str)
            (cond
              [(empty? low) BLANK]
              [else
               (above/align "left" (render-w (first low) str)
                            (render-low (rest low) str))]))]
    (render-w w "")))

;; widget -> string
;; returns a color based on the quantity of a widget
;; red if <5 and yellow if <10
(define (quantity->color w)
  (if (< (widget-quantity w) 10)
      (if (< (widget-quantity w) 5)
          "red"
          "gold") ;yellow is impossible to see, so I went with gold
      TEXT-COLOR))

;; widget -> string
;; returns a color based on the price of a widget
;; green if <$5 and magenta if >$15
(define (price->color w)
  (cond [(< (widget-price w) 5) "green"]
        [(> (widget-price w) 15) "magenta"]
        [else TEXT-COLOR]))

;; ============================================================
;;Examples
(simple-render Jewelry)
(text "" TEXT-SIZE TEXT-COLOR) ;for spacing

(render Jewelry quantity->color)
(text "" TEXT-SIZE TEXT-COLOR) ;for spacing

(render Jewelry price->color)
