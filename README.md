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
