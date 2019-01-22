#lang racket
;; Use the trick from previous chapter: avoid passing non-recur variable and hide function
;; We have new tricks: use `call/cc` to return value abruptly and promptly
(define intersect
  (lambda (set1 set2)
    (letrec ((I (lambda (set)
                  (cond
                    ((null? set) '())
                    ((member (car set) set2)
                     (cons (car set)
                           (I (cdr set))))
                    (else (I (cdr set)))))))
      (I set1))))

(intersect '(tomatoes and macaroni) '(macaroni and cheese))

(define intersectall
  (lambda (lset)
    (call/cc
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                   ((null? (car lset))
                    (hop '()))
                   ((null? (cdr lset))
                    (car lset))
                   (else
                    (intersect (car lset)
                              (A (cdr lset))))))))
         (cond
           ((null? lset) '())
           (else (A lset))))))))

(intersectall '((3 mangos and) (3 kiwis and) (3 hamburgers)))
(intersectall '((3 mangos and) () (3 hamburgers)))
