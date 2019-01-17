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

;; letrec example
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
      m-f)))

;; 2 ways of defining `member?`
(define member1?
  (lambda (a lat)
    ((letrec ((yes? (lambda (l)
                     (cond
                       ((null? l) #f)
                       ((eq? (car l) a) #t)
                       (else (yes? (cdr l)))))))
       yes?)
     lat)))

(define member2?
  (lambda (a lat)
    (letrec ((yes? (lambda (l)
                     (cond
                       ((null? l) #f)
                       ((eq? (car l) a) #t)
                       (else (yes? (cdr l)))))))
      (yes? lat))))

(member1? 1 '(3 2 1 0))
(member2? 1 '(3 2 1 0))


;; Since `set2` will not change in recursion, we use the same trick again.
;; To hide and protect `member?`, we don't define it in global bindings.
;; We put it in `letrec` too.
;; Notice that we can also apply the trick to `M?`.
(define union
  (lambda (set1 set2)
    (letrec ((U (lambda (set)
                  (cond
                    ((null? set) set2)
                    ((M? (car set) set2) (U (cdr set)))
                    (else (cons (car set)
                                (U (cdr set)))))))
             (M? (lambda (a lat)
                  (letrec ((N? (lambda (lat)
                                 (cond
                                   ((null? lat) #f)
                                   ((eq? (car lat) a) #t)
                                   (else (N? (cdr lat)))))))
                    (N? lat)))))
      (U set1))))

(union '(1 2 3) '(5 3 7))
