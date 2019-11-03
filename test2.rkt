;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define add2
  (lambda (x) (+ x 1)))


(add2 15)

(define curry
  (lambda (procedure arg1)
    (lambda (arg2)
      (procedure arg1 arg2))))

(define add3 (curry + 1))



  

