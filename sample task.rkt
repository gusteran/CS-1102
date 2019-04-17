;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |sample task|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct person (name children))
;; a person is a (make-person String ListOfPerson)

;; YOUR TASK
;; write a function, all-descendents, that consumes a person
;; and returns a list of the names of all descendents of that
;; person -- including the person himself (the last part makes
;; your job a bit easier)

(define p1 (make-person "bob" empty))
(define p2 (make-person "chris" empty))
(define p3 (make-person "pat" (list p2)))
(define p4 (make-person "reagan" (list p1 p3)))

(check-expect (all-descendents p1) (list "bob"))

;; the sort in this check-expect is to allow student solutions that
;; visit the persons in a different order than our solution.
;; Your solution should NOT involve sorting.
;; Our solution returned (list "reagan" "bob" "pat" "chris").
;; Returning (list "chris" "bob" "reagan" "pat") would be odd, but is
;; acceptable :-)
(check-expect (sort (all-descendents p4) string<=?)
              (list "bob" "chris" "pat" "reagan"))

(check-expect (sort (all-descendents p3) string<=?)
              (list "chris" "pat"))

;; person -> ListOfPerson
(define (all-descendents per)
  (cons (person-name per) (all-lod (person-children per))))

(define (all-lod lod)
  (cond [(empty? lod) empty]
        [else (append (all-descendents (first lod))
                      (all-lod (rest lod)))]))


;; (you will need to leave ISL-lambda enabled for this problem
;; so that our test cases work properly.  you do NOT need to
;; use any constructs from ISL-lambda in your solution, but can
;; if you wish)