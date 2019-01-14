#lang racket

(define is-first-b?
  (lambda (a lat)
    (if (null? lat)
        #f
        (or (eq? (car lat) a)
            (two-in-a-row? lat)))))

(define two-in-a-row?
  (lambda (lat)
    (if (null? lat)
        #f
        (is-first-b? (car lat) (cdr lat)))))

(two-in-a-row? '(Italian sardines sardines spaghetti parslet))



