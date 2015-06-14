## ANN-racket
An [Artificial Neural Network](https://en.wikipedia.org/wiki/Artificial_neural_network) is a machine learning model. I learned about it on [Math Intersect Programming](http://jeremykun.com/2012/12/09/neural-networks-and-backpropagation/).

### How to use code
`neural.rkt` provides a function `make-network`.

1. Call `make-network` to get an instance of `network%`
2. `send` `train` on this instance with your training data
3. `evaluate` your test data as needed

Look at `binary-example.rkt` and `sin-example.rkt` for more info
