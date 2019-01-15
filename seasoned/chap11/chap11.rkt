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


;; The same trick
(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      ((null? tup) '())
      (else (cons (+ sonssf (car tup))
                  (sum-of-prefixes-b (+ sonssf (car tup))
                                     (cdr tup)))))))
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(sum-of-prefixes '(1 1 1))


;; Now the additional information is all reversed prefixes
(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else
       (let* ((cur (car tup))
              (new-pre (cons cur rev-pre)))
         (cons (pick cur new-pre)
               (scramble-b (cdr tup) new-pre)))))))
(define pick
  (lambda (n lat)
    (cond
      ((eq? n 1) (car lat))
      (else (pick (- n 1) (cdr lat))))))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))

(scramble '(1 1 1 3 4 2 1 1 9 2))


(define multirember-f
  (lambda (test?)
    (letrec
        ((m-f
          (lambda (a lat)
            (cond
              ((null? lat) '())
              ((test? (car lat) a)
               (m-f a (cdr lat)))
              (else
               (cons (car lat)
                     (m-f a (cdr lat))))))))
      mf)))
