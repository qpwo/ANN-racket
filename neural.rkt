#lang racket
; Luke Miles, June 2015


(define-syntax cons!
  (syntax-rules ()
    ((_ x ls)
     (set! ls (cons x ls)))))

(define-syntax set-field-op!
  (syntax-rules ()
    [(_ id obj-expr op-expr expr)
     (let ([object obj-expr])
       (set-field! id object
         (op-expr expr (get-field id object))))]))

(define (activation-function x)
  (/ 1. (+ 1. (exp (- x)))))

(define node%
  (class object%
    (super-new)
    ; fields with * don't need to be fields.
    ; just makes it easier to debug.
    (field [last-input (void)] ;*
           [last-output (void)]
           [incoming-edges null]
           [outgoing-edges null]
           [the-error (void)] ;*
           [evaluate-cache (void)]) ;*
    (add-bias)

    (define/public (add-bias)
      (cons! (make-object edge% (new bias-node%) this)
             incoming-edges))

    (define/public (evaluate input-vector)
      (if (not (void? last-output))
        last-output
        (let-values
          ([(new-input weighted-sum)
            (for/fold ([new-input null]
                       [weighted-sum 0])
                      ([e (in-list incoming-edges)])
              (let ([cur-input (send (get-field source e) evaluate input-vector)])
                (values (cons cur-input new-input)
                        (+ weighted-sum (* (get-field weight e) cur-input)))))])
          (set! last-input new-input)
          (set! last-output (activation-function weighted-sum))
          (set! evaluate-cache last-output)
          last-output)))

    (define/public (get-error label)
      (if (not (void? the-error))
        the-error
        (begin
          (when (void? last-output) (error "last-output is void"))
          (set! the-error
            (if (null? outgoing-edges)
              (- label last-output)
              (for/sum ([edge (in-list outgoing-edges)])
                (* (get-field weight edge)
                   (send (get-field target edge) get-error label)))))
          the-error)))

    (define/public (update-weights learning-rate)
      (when (nor (void? the-error)
                 (void? last-output)
                 (void? last-input))

        (for ([edge (in-list incoming-edges)]
              [input (in-list last-input)])
          (set-field-op! weight edge +
            (* learning-rate last-output
               (- 1 last-output) the-error input)))

        (for ([edge (in-list outgoing-edges)])
          (send (get-field target edge) update-weights learning-rate))
        (set! the-error (void))
        (set! last-input (void))
        (set! last-output (void))))

    (define/public (clear-evaluate-cache)
      (when (not (void? last-output))
        (set! last-output (void))
        (for ([edge (in-list incoming-edges)])
          (send (get-field source edge) clear-evaluate-cache))))))

(define input-node%
  (class node%
    (super-new)
    (inherit-field last-output outgoing-edges)
    (init-field index)

    (define/override (evaluate input-vector)
      (set! last-output (list-ref input-vector index))
      last-output)

    (define/override (update-weights learning-rate)
      (for ([edge (in-list outgoing-edges)])
        (send (get-field target edge) update-weights learning-rate)))

    (define/override (get-error label)
      (for ([edge (in-list outgoing-edges)])
        (send (get-field target edge) get-error label)))

    (define/override (add-bias)
      (void))

    (define/override (clear-evaluate-cache)
      (set! last-output (void)))))

(define bias-node%
  (class input-node%
    (super-new [index -1])

    (define/override (evaluate input-vector)
      1.)))

(define edge%
  (class object%
    (super-new)
    (init-field source target)
    (field [weight (random)])
    (set-field-op! outgoing-edges source cons this)
    (set-field-op! incoming-edges target cons this)))

(define network%
  (class object%
    (super-new)
    (field [input-nodes null]
           [output-node (void)])

    (define/public (evaluate input-vector)
      (unless (< (apply max (map (λ (node) (get-field index node))
                                 input-nodes))
                 (length input-vector))
        (error "input vector not long enough"))
      (send output-node clear-evaluate-cache)
      (send output-node evaluate input-vector))

    (define (propagate-error label)
      (for ([node (in-list input-nodes)])
        (send node get-error label)))

    (define (update-weights learning-rate)
      (for ([node (in-list input-nodes)])
        (send node update-weights learning-rate)))

    (define/public (train labeled-points
                          [learning-rate .9]
                          [max-iterations 10000])
      (do ([i 0])
          (> i max-iterations)
        (for ([labeled-point (in-list labeled-points)])
            (evaluate (cdr labeled-point))
            (propagate-error (car labeled-point))
            (update-weights learning-rate)
            (set! i (add1 i)))))))

(define (make-listf k f)
  (let M ([k k])
    (if (zero? k) null (cons (f) (M (sub1 k))))))

(define (make-network num-inputs num-hidden-layers num-in-each-layer)
  (define network (new network%))
  (define input-nodes (for/list ([i (in-range num-inputs)])
                        (new input-node% [index i])))
  (define output-node (new node%))
  (set-field! output-node network output-node)
  (set-field-op! input-nodes network append input-nodes)
  
  (define layers
    (make-listf num-hidden-layers
      (λ () (make-listf num-in-each-layer
              (λ () (new node%))))))
  
  (for* ([input-node (in-list input-nodes)]
         [node (in-list (first layers))])
    (new edge% [source input-node] [target node]))

  (for ([layer1 (in-list layers)]
        [layer2 (in-list (rest layers))])
    (for* ([node1 (in-list layer1)]
           [node2 (in-list layer2)])
      (new edge% [source node1] [target node2])))

  (for ([node (in-list (last layers))])
    (make-object edge% node output-node))

  network)

(provide make-network)
