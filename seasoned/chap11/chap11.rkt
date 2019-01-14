#lang racket

;; `preceding` stores some current state of the computation,
;; while `lat` is used to recur on.
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) preceding)
                (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))

(two-in-a-row? '(Italian sardines sardines spaghetti parslet))


(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      ((null? tup) '())
      (else (cons (+ sonssf (car tup))
                  (sum-of-prefixes-b (+ sonssf (car tup))
                                     (cdr tup)))))))
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes 0 tup)))

(sum-of-prefixes '(1 1 1))
