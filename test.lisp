(defpackage #:org.shirakumo.random-sampling.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:random #:org.shirakumo.random-state)
   (#:sampling #:org.shirakumo.random-sampling)))
(in-package #:org.shirakumo.random-sampling.test)

(define-test random-sampling)

(defmacro test-distribution-generic (distribution &rest args)
  `(let* ((sample (of-type 'single-float (,distribution ,@args)))
          (pdf (of-type '(or function null) (sampling:pdf ',distribution NIL))))
     (when pdf
       (of-type '(single-float 0.0 1.0) (funcall pdf sample ,@args)))))

(defmacro define-default-distribution-test (type args &body body)
  `(define-test ,type
     :parent random-sampling
     (let ((random:*generator* (random:make-generator :squirrel NIL)))
       ,@body
       (test-distribution-generic ,type ,@args))))

(define-default-distribution-test sampling:gaussian ())
(define-default-distribution-test sampling:gaussian-tail (1.5))
(define-default-distribution-test sampling:exponential (1.0))
(define-default-distribution-test sampling:laplace (1.0))
(define-default-distribution-test sampling:cauchy (1.0))
(define-default-distribution-test sampling:rayleigh (1.0))
(define-default-distribution-test sampling:rayleigh-tail (1.0 1.0))
(define-default-distribution-test sampling:levy (1.0 1.0))
(define-default-distribution-test sampling:levy-skew (1.0 1.0 1.0))
(define-default-distribution-test sampling:gamma (1.0))
(define-default-distribution-test sampling:flat (0.5 2.5))
(define-default-distribution-test sampling:lognormal (0.0 1.0))
(define-default-distribution-test sampling:chi-squared (1.0))
(define-default-distribution-test sampling:fdist (1.0 1.0))
(define-default-distribution-test sampling:tdist (1.0))
(define-default-distribution-test sampling:beta (2.0 2.0))
(define-default-distribution-test sampling:logistic (1.0))
(define-default-distribution-test sampling:pareto (1.0 1.0))
(define-default-distribution-test sampling:weibull (1.0 1.0))
(define-default-distribution-test sampling:gumbel-1 (1.0 1.0))
(define-default-distribution-test sampling:gumbel-2 (1.0 1.0))
(define-default-distribution-test sampling:poisson (2.5))
(define-default-distribution-test sampling:bernoulli (0.7))
(define-default-distribution-test sampling:binomial (0.5 10.0))
(define-default-distribution-test sampling:-binomial (0.5 3.5))
(define-default-distribution-test sampling:geometric (0.5))
(define-default-distribution-test sampling:logarithmic (0.7))
