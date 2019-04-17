;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |sample task 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct item (price volume name))
;; an item is a (make-item Number Number String)

;; YOUR TASK
;; write a function that consumes a ListOfItem as well as a threshold ratio.
;; it returns all the items whose ratio of price/volume falls below the threshold
;; for the items that meet the threshold, it creates a little advertising slogan
;; for that item (see the check-expects for details)

(define loi
  (list (make-item 1.45 67.6 "soda")
        (make-item 2.99  16 "fancy water")
        (make-item 1.68  32 "milk")
        (make-item 9.99 2.5 "beef")
        (make-item 3.65 999 "gum drops")))

(check-expect (advertise-cheap loi 0.1)
              (list "Buy some cheap soda." "Buy some cheap milk." "Buy some cheap gum drops."))

(check-expect (advertise-cheap loi 0.0) empty)

(check-expect (advertise-cheap loi 4)
              (list
               "Buy some cheap soda."
               "Buy some cheap fancy water."
               "Buy some cheap milk."
               "Buy some cheap beef."
               "Buy some cheap gum drops."))

(check-expect (advertise-cheap empty 0) empty)

;; Item -> Number
;;helper to get ratios
(define (ratio-p/v item)
  (/ (item-price item) (item-volume item)))


;; ListOfItem Number -> ListOfString
(define (advertise-cheap loi ratio)
  (cond [(empty? loi) empty]
        [(< (ratio-p/v (first loi)) ratio)
         (cons (string-append "Buy some cheap " (item-name (first loi)) ".")
               (advertise-cheap (rest loi) ratio))]
        [else (advertise-cheap (rest loi) ratio)]))