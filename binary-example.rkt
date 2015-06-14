#lang racket

(require "neural.rkt")

(define network (make-network 3 1 3))

(define labeled-points
  '(((0 0 0) . 1)
    ((0 0 1) . 0)
    ((0 1 0) . 1)
    ((0 1 1) . 0)
    ((1 0 0) . 1)
    ((1 0 1) . 0)
    ((1 1 0) . 1)
    ((1 1 1) . 0)))

(send network train labeled-points 0.9 5000)
;(define lp (third labeled-points))

(for ([point (map car labeled-points)])
  (displayln (list (send network evaluate point)
                   point)))
