#lang racket
;; Use the trick from previous chapter: avoid passing non-recur variable and hide function
;; We have new tricks: use `call/cc` to return value abruptly and promptly
;; Here We remove all unnecessary logic
(define intersectall
  (lambda (lset)
    (let/cc hop
      (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset))
                   (hop '()))
                  ((null? (cdr lset))
                   (car lset))
                  (else (I (car lset)
                           (A (cdr lset)))))))
           (I (lambda (s1 s2)
                (letrec
                    ((J (lambda (s1)
                          (cond
                            ((null? s1) '()) ;; actually this case will be handled in outer logic
                            ((member (car s1) s2)
                             (cons (car s1)
                                        (J (cdr s1))))
                            (else (J (cdr s1)))))))
                  (cond
                    ((null? s2) (hop '()))
                    (else (J s1)))))))
        (cond
          ((null? lset) '())
          (else (A lset)))))))

(intersectall '((3 mangos and) (3 kiwis and) (3 hamburgers)))
(intersectall '((3 mangos and) () (3 hamburgers)))
(intersectall '((3 steaks and) (no food and) (three baked potatoes) (3 diet hamburgers)))
