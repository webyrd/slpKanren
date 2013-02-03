;;; Stochastic Logic Programs (SLP) style probabilistic logic programming in miniKanren,
;;; based on Stephen Muggleton's paper, 'Stochastic Logic Programs':
;;; http://www.doc.ic.ac.uk/~shm/Papers/slp.pdf

;;; Code by Rebecca Swords and William E. Byrd, based on core miniKanren.

;;; slpKanren extends core miniKanren with one relational operator
;;; (condp) and two interface operators (run-prob and run-prob*):
;;;
;;;
;;; (condp [prob-exp g g* ...] ...)
;;;
;;; condp is identitical to conde, except that the first expression in
;;; each clause must evaluate to a real number representing the
;;; probability associated with that clause.  Operationally, condp
;;; behaves identically to conde, other than associating a probability
;;; with each successful clause.  In other words, condp and conde
;;; produce the same answers, in the same order; however, condp
;;; associates a probility with each answer.
;;;
;;;
;;; (run-prob n (x) g0 g ...)
;;; (run-prob* (x) g0 g ...)
;;;
;;; run-prob and run-prob* are identical to run and run*, except that
;;; the probability associated with each answer is also returned.

;;; This implementation also includes two debugging goals, which can
;;; be used to examine the substition: print-substo and
;;; print-prob-substo.


;;; debugging macro, originally designed by Aziz Ghuloum
#;(define-syntax lambda
  (let ()
    (import scheme)
    (syntax-rules ()
      [(_ args b b* ...)
       (case-lambda
         [args b b* ...]
         [others (error 'apply (format "incorrect args ~s for ~s"
                                       others '(lambda args b b* ...)))])])))

(load "pmatch.scm")
(load "mkdefs.scm")

;;; debugging goals
(define print-substo
  (lambda (s)
    (begin
      (printf "substo: ~s\n" s)
      ((== #t #t) s))))

(define print-prob-substo
  (lambda (s)
    (begin
      (printf "~s\n" (calc-probs s))
      ((== #t #t) s))))


;;; implementation

(define-syntax condp
  (syntax-rules ()
    [(_ [prob-exp g g* ...] ...)
     (conde
       [(exist (p)
          (let ([prob prob-exp])
            (== (cons 'probability prob) p)))
        g g* ...]
       ...)]))

(define calc-probs
  (lambda (s)
    (apply
      *
      (map
        (lambda (a) (pmatch a
                 [(,var . (probability . ,prob)) prob]
                 ;;; is using 1.0 here legitimate?  should we modify 'exist' instead?
                 [,else 1.0]))
        s))))

;; now a helper
(define-syntax run-prob
  (syntax-rules ()
    [(_ n (x) g0 g ...)
     (take n
           (lambdaf@ ()
             ((exist (x) g0 g ...
                     (lambdag@ (s)
                       (cons (cons (reify x s) (calc-probs s)) '())))
              empty-s)))]))

(define-syntax run-prob*
  (syntax-rules ()
    ((_ (x) g ...)
     (let ([ans (run-prob #f (x) g ...)])
       (sum-same ans)))))

(define sum-same (lambda (ls) (reverse (sum-same-aux ls '()))))

(define sum-same-aux
  (lambda (ls acc)
    (cond
      [(null? ls) acc]
      [(assq (caar ls) acc) =>
       (lambda (p)
         (sum-same-aux (cdr ls)
           (cons (cons (caar ls) (+ (cdar ls) (cdr p))) (remq p acc))))]
      [else (sum-same-aux (cdr ls) (cons (car ls) acc))])))

;; uncomment this macro to make condp
;; behave like conde
;(define-syntax condp
;  (syntax-rules ()
;    [(_ [prob-exp g g* ...] ...)
;     (conde [g g* ...] ...)]))
