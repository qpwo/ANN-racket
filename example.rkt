#lang racket

(require "neural.rkt")

(define ins (for/list ([i (in-range 3)])
              (new input-node% [index i])))

(define m1 (make-listf 3 (λ () (new node%))))

(define out (new node%))

(for* ([in-node ins]
       [middle1 m1])
  (make-object edge% in-node middle1))


(for ([middle1 m1])
  (make-object edge% middle1 out))

(define network (new network%))

(set-field! input-nodes network ins)
(set-field! output-node network out)

(define labeled-points
  '((1 0 0 0)
    (0 0 0 1)
    (1 0 1 0)
    (0 0 1 1)
    (1 1 0 0)
    (0 1 0 1)
    (1 1 1 0)
    (0 1 1 1)))

(send network train labeled-points 0.9 5000)
;(define lp (third labeled-points))

(for ([point (map cdr labeled-points)])
  (displayln (list (send network evaluate point)
                   point)))
