;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(string-length "hello world")

;(string-append "bloody " (substring "hello world" 0 4))

(require 2htdp/image)
;(overlay (circle 10 "solid" "red")
;(rectangle 50 100 "outline" "blue")
;(text "hello" 30 "yellow"))

;;function-definitions-starter.rkt

(define (bulb c)
(circle 40 "solid" c))

(above (bulb "purple") (bulb "orange") (bulb "blue") (bulb "red"))
