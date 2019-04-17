;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment4Part1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; written by Gus Teran and Adrian Curless

;(require racket/list)
(require 2htdp/image)


(define TEXT-SIZE 20)    
(define TEXT-COLOR "black") 
(define TAB 5)

(define-struct widget (name quantity price))
;; a widget is a (make-widget String Natural Number)
; same as assignment #3, except no parts component

(define-struct bst (widget left right))
;; a BST is either
;;          false, or
;;          a (make-bst widget bst bst)

;template for bst
(define (fn-for-bst bst)
  (cond [(false? bst) ...]
        [else
         (... widget
              (fn-for-bst (bst-left bst))
              (fn-for-bst (bst-right bst)))]))


;; returns a random natural  between 1 and max, inclusive
;; Natural -> Natural
(define (rnd max)
  (add1 (random max)))

;; Natural Natural -> (listof widget)
;; creates a list num random widgets whose values vary between 1 and max
(define (random-widgets num max)
  (build-list num
              (lambda (dummy)
                (make-widget 
                 (number->string (rnd max))
                 (rnd max)
                 (rnd max)))))

;; (listof widget) bst -> bst
;; inserts all widgets in low into bst
(define (build-tree low)
  (foldr insert-name false low))
 
;; widget bst -> bst
;; returns the given bst with the given widget added to it
(define (insert-name w bst)
  (cond [(false? bst) (make-bst w false false)]
        [(string>?  (widget-name w) (widget-name (bst-widget bst))) 
         (make-bst (bst-widget bst) (bst-left bst) (insert-name w (bst-right bst)))]
        [else
         (make-bst (bst-widget bst) (insert-name w (bst-left bst)) (bst-right bst))]))

(check-expect (insert-name Beads false) (make-bst Beads false false))
(check-expect (insert-name Beads
                           (make-bst Glass
                                     false
                                     (make-bst (make-widget "Test" 10 10) false false)))
              (make-bst Glass
                        (make-bst Beads
                                  false
                                  false)
                        (make-bst (make-widget "Test" 10 10) false false)))
(check-expect (insert-name (make-widget "A" 10 10)
                           (make-bst Glass
                                     (make-bst Beads
                                               false
                                               false)
                                     (make-bst (make-widget "Test" 10 10) false false)))
              (make-bst Glass
                        (make-bst Beads
                                  (make-bst (make-widget "A" 10 10)
                                            false
                                            false)
                                  false)
                        (make-bst (make-widget "Test" 10 10) false false)))

;; here is some code related to displaying a tree
;; you might find it helpful for debugging
;; (render bst) -> image


;; helper functions, can ignore
(define (blanks n)
  (list->string (build-list n (lambda (x) #\ ))))

(define (to-text side w t)
  (text  (string-append (blanks t) side (widget-name w)) TEXT-SIZE TEXT-COLOR))

(define (render-helper b t img side)
  (if (false? b)
      img
      (above/align "left"
                   (to-text side (bst-widget b) t)
                   (render-helper (bst-left b) (+ t TAB) img "L: ")
                   (render-helper (bst-right b) (+ t TAB) img "R: "))))
;; end of helper functio

;; render:  BST -> image
;; provides a graphical representation of the tree
(define (render b)
  (render-helper b 0 (square 0 "solid" "white") "T: "))
;;========================================================================
(define Glass (make-widget "Glass" 10 5))
(define Beads (make-widget "Beads" 15 20))


(define bst1 (build-tree (list Glass Beads)))

;; string bst -> Widget | false
;; returns a widget with the given name if it is in the given bst, otherwise returns false
;(define (find-name name bst) false) ;stub
(check-expect (find-name "Beads" bst1) Beads)
(check-expect (find-name "Glass" bst1) Glass)
(check-expect (find-name "Test" (make-bst Glass
                                          (make-bst Beads
                                                    (make-widget "A" 10 10)
                                                    false)
                                          (make-bst (make-widget "Test" 10 10) false false)))
              (make-widget "Test" 10 10))
(check-expect (find-name "A" (make-bst Glass
                                       (make-bst Beads
                                                 (make-bst (make-widget "A" 10 10) false false)
                                                 false)
                                       (make-bst (make-widget "Test" 10 10) false false)))
              (make-widget "A" 10 10))
(check-expect (find-name "Something" false) false)


(define (find-name name bst)
  (cond [(false? bst) false]
        [(string=? name (widget-name (bst-widget bst))) (bst-widget bst)]
        [(string>?  name (widget-name (bst-widget bst))) 
         (find-name name (bst-right bst))]
        [else (find-name name (bst-left bst))]))


