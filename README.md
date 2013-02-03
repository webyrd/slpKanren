slpKanren
=========

Stochastic Logic Programs (SLP) style probabilistic logic programming in miniKanren, based on Stephen Muggleton's paper, 'Stochastic Logic Programs': http://www.doc.ic.ac.uk/~shm/Papers/slp.pdf

Code by Rebecca Swords and William E. Byrd, based on core miniKanren.

slpKanren extends core miniKanren with one relational operator (```condp```) and two interface operators (```run-prob``` and ```run-prob*```):

```
(condp [prob-exp g g* ...] ...)
```

```condp``` is identitical to ```conde```, except that the first expression in each clause must evaluate to a non-negative real number representing the probability associated with that clause.

```
(run-prob n (x) g0 g ...)
(run-prob* (x) g0 g ...)
```

```run-prob``` and ```run-prob*``` are identical to ```run``` and ```run*```, except that the probability associated with each answer is also returned.

This implementation also includes two debugging goals, which can be used to examine the substition: ```print-substo``` and ```print-prob-substo```.

Example slpKanren program, adapted from the Muggleton paper:

```
;;; stochastic automaton
(define sa
  (lambda (S)
    (letrec ([q0 (lambda (S)
                   (exist (S^)
                     (condp
                       [0.4
                        (== `(a . ,S^) S)
                        (q0 S^)]
                       [0.6
                        (== `(b . ,S^) S)
                        (q1 S^)])))]
             [q1 (lambda (S)
                   (exist (S^)
                     (condp
                       [0.7
                        (== `(b . ,S^) S)
                        (q1 S^)]
                       [0.3
                        (== `(c . ,S^) S)
                        (q2 S^)])))]
             [q2 (lambda (S) (== '() S))])
      (q0 S))))
```

and associated test cases:

```
(test "sa-1"
  (run-prob 10 (q) (sa q))
  '(((b c) . 0.18)
    ((b b c) . 0.126)
    ((a b c) . 0.072)
    ((b b b c) . 0.08819999999999999)
    ((b b b b c) . 0.06173999999999999)
    ((a b b c) . 0.0504)
    ((b b b b b c) . 0.043217999999999986)
    ((b b b b b b c) . 0.03025259999999999)
    ((a a b c) . 0.0288)
    ((a b b b c) . 0.03528)))
```

and

```    
(test "sa-2"
  (run-prob 1 (q) (== '(a b b c) q) (sa q))
  `(((a b b c) . ,(* 0.4 0.6 0.7 0.3))))
```

All Scheme code tested under Petite Chez Scheme Version 8.4.