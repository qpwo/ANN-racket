#lang racket
(require "neural.rkt")

(define (my-sin x) ; sin in range [.1 .9] for easier copying by ANN
  (+ .5 (* .4 (sin x))))

(define labeled-points
  (for/list ([__ (in-range 100)])
    (let ([x (* 4 pi (random))])
      (cons (list x) (my-sin x)))))

(define network (make-network 1 1 20))

(send network train labeled-points .25 100000)

(define (neural-sin x)
  (/ (- (send network evaluate (list x)) .5) .4))
