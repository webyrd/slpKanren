(load "slpKanren.scm")

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (let* ((expected expected-result)
            (produced tested-expression))
       (if (equal? expected produced)
           (printf "~s works!\n" title)
           (error
            'test
            (format "Failed ~s: ~a\nExpected: ~a\nComputed: ~a"
                    title 'tested-expression expected produced)))))))

;;; Quantum computing related tests

(define Hadamardo
  (lambda (c d)
    (condp
      [(/ (sqrt 2))
       (== #f c)
       (== #f d)]
      [(/ (sqrt 2))
       (== #f c)
       (== #t d)]
      [(/ (sqrt 2))
       (== #t c)
       (== #f d)]
      [(- (/ (sqrt 2)))
       (== #t c)
       (== #t d)])))

(define Identityo
; assuming c is #f, then the first call to Hadamardo will associate e
; with the superposition:
; e = #f 1/sqrt 2 | #t 1/sqrt 2
; Passing this value of e into Hardamardo once again will produce
; e = #f 1/sqrt 2, d = #f 1/2 | d = #t 1/2
; e = #t 1/sqrt 2, d = #f 1/2 | d = #t -1/2
; summing these probabilities for d gives d = #f 1 | d = #t 0
; In other words, d = #f deterministically.
  (lambda (c d)
    (exist (e)
      (Hadamardo c e)
      (Hadamardo e d))))

(test "sum-same-1"
  (sum-same
   '((#f . 0.4999999999999999)
     (#t . 0.4999999999999999)
     (#f . 0.4999999999999999)
     (#t . -0.4999999999999999)))
  `((#f . ,(* 2 0.4999999999999999))
    (#t . ,(+ 0.4999999999999999 -0.4999999999999999))))

(test "Identityo-1"
  (run-prob* (q) (Identityo #f q))
  '((#f . 0.9999999999999998) (#t . 0.0)))

(test "Identityo-2"
  (run-prob* (q) (Identityo #t q))
  '((#f . 0.0) (#t . 0.9999999999999998)))

(test "Hadamardo-1"
  (run-prob* (q) (Hadamardo #f q))
  '((#f . 0.7071067811865475) (#t . 0.7071067811865475)))

(test "Hadamardo-2"
  (run-prob* (q) (Hadamardo #t q))
  '((#f . 0.7071067811865475) (#t . -0.7071067811865475)))

;;; SLP examples taken from Stephen Muggleton's 1996 paper,
;;; 'Stochastic Logic Programs'
;;; www.doc.ic.ac.uk/~shm/Papers/slp.pdf

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

;;; stochastic context-free grammar
(define scfg
  (lambda (S)
    (condp
      [0.5 (== '() S)]
      [0.5
       (exist (S^)
         (== `(a ,S^ b) S)
         (scfg S^))])))

;;; stochastic logic programs

(define coino
  (lambda (c)
    (condp
      [0.5 (== c 0)]
      [0.5 (== c 1)])))

(define nateo
  (lambda (n)
    (condp
      [0.5 (== n 'z)]
      [0.5
       (exist (n-1)
         (== `(s ,n-1) n)
         (nateo n-1))])))

(define natpo
  (lambda (n)
    ;; seems weird to place a probability of 1.0
    ;; on the exist--perhaps this can be implied
    (exist (u)
      (nateo u)
      (bino u n))))

(define bino
  (lambda (len n)
    (condp
      [0.5 (== 'z len) (== '(1) n)]
      [0.5
       (exist (u c n-1)
         (== `(s ,u) len)
         (== `(,c . ,n-1) n)
         (coino c)
         (bino u n-1))])))

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

(test "sa-2"
  (run-prob 1 (q) (== '(a b b c) q) (sa q))
  `(((a b b c) . ,(* 0.4 0.6 0.7 0.3))))

(test "scfg-1"
  (run-prob 8 (q) (scfg q))
  '((() . 0.5)
    ((a () b) . 0.25)
    ((a (a () b) b) . 0.125)
    ((a (a (a () b) b) b) . 0.0625)
    ((a (a (a (a () b) b) b) b) . 0.03125)
    ((a (a (a (a (a () b) b) b) b) b) . 0.015625)
    ((a (a (a (a (a (a () b) b) b) b) b) b) . 0.0078125)
    ((a (a (a (a (a (a (a () b) b) b) b) b) b) b) . 0.00390625)))

(test "scfg-2"
  (run-prob* (q)
    (== q '(a (a () b) b))
    (scfg q))
  '(((a (a () b) b) . 0.125)))

(test "coino-1"
  (run-prob* (q) (coino q))
  '((0 . 0.5)
    (1 . 0.5)))

(test "nateo-1"
  (run-prob 3 (q) (nateo q))
  '((z . 0.5)
    ((s z) . 0.25)
    ((s (s z)) . 0.125)))

(test "natpo-1"
  (run-prob 10 (q) (natpo q))
  '(((1) . 0.25)
    ((0 1) . 0.03125)
    ((1 1) . 0.03125)
    ((0 0 1) . 0.00390625)
    ((1 0 1) . 0.00390625)
    ((0 1 1) . 0.00390625)
    ((1 1 1) . 0.00390625)
    ((0 0 0 1) . 4.8828125e-4)
    ((1 0 0 1) . 4.8828125e-4)
    ((0 1 0 1) . 4.8828125e-4)))

;;; PRISM example taken from:
;;; 'Logic-based Probabilistic Modeling', Taisuke Sato, WoLLIC 2009
;;; http://sato-www.cs.titech.ac.jp/reference/Sato-WoLLIC09.pdf

(define bloodtypeo
  (lambda (p)
    (exist (x y)
      (condp
        [0.5 (== x 'a)
         (condp
           [0.5 (== y 'a) (== p 'a)]
           [0.2 (== y 'b) (== p 'ab)]
           [0.3 (== y 'o) (== p 'a)])]
        [0.2 (== x 'b)
         (condp
           [0.5 (== y 'a) (== p 'ab)]
           [0.2 (== y 'b) (== p 'b)]
           [0.3 (== y 'o) (== p 'b)])]
        [0.3 (== x 'o)
         (condp
           [0.5 (== y 'a) (== p 'a)]
           [0.2 (== y 'b) (== p 'b)]
           [0.3 (== y 'o) (== p 'o)])]))))

(test "run-prob-1" (run-prob* (q) (== 'a q) (bloodtypeo q)) '((a . 0.55)))
(test "run-prob-2" (run-prob* (q) (== 'b q) (bloodtypeo q)) '((b . 0.16)))
(test "run-prob-3" (run-prob* (q) (== 'ab q) (bloodtypeo q)) '((ab . 0.2)))
(test "run-prob-4" (run-prob* (q) (== 'o q) (bloodtypeo q)) '((o . 0.09)))

(test "run-prob-5"
  (run-prob* (q) (bloodtypeo q))
  '((ab . 0.2) (a . 0.55) (b . 0.16) (o . 0.09)))

(test "run-prob-6"
  (let ([prob (apply + (map cdr (run-prob* (x) (bloodtypeo x))))])
    (and (> prob 0.9999) (< prob 1.00001)))
  #t)
