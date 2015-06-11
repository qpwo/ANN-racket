#lang racket

(define (make-listf k f)
  (let M ([k k])
    (if (zero? k) null (cons (f) (M (sub1 k))))))

(define (activation-function x)
  (/ 1. (+ 1. (exp (- x)))))

(define node%
  (class object%
    (super-new)
    (init num-in)

    (field [weights (make-listf num-in random)])

    (define/public (evaluate point)
      (activation-function
        (for/sum ([w weights]
                  [x point])
          (* w x))))

    (define/public (adjust labeled-point rate)
      (define correct (car labeled-point))
      (define guess (evaluate (cdr labeled-point)))
      (set! weights
        (for/list ([w weights]
                   [x (cdr labeled-point)])
          (+ w (* rate guess (- 1 guess)
                  (- correct guess) x)))))))

(define labeled-points
  '((1 0 0 0)
    (0 0 0 1)
    (1 0 1 0)
    (0 0 1 1)
    (1 1 0 0)
    (0 1 0 1)
    (1 1 1 0)
    (0 1 1 1)))

(define john (new node% [num-in 3]))

(for ([point (in-cycle (in-list labeled-points))]
      [__ (in-range 10000)])
  (send john adjust point .9))

(map (λ (point) (list (send john evaluate point)
                      point))
     (map cdr labeled-points))
