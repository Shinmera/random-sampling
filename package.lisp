(defpackage #:org.shirakumo.random-sampling
  (:use #:cl #:org.shirakumo.fraf.math)
  (:shadowing-import-from #:org.shirakumo.random-state #:random)
  (:local-nicknames
   (#:random-state #:org.shirakumo.random-state))
  ;; distributions.lisp
  (:export
   #:gaussian
   #:gaussian-pdf
   #:gaussian-tail
   #:gaussian-tail-pdf
   #:exponential
   #:exponential-pdf
   #:laplace
   #:laplace-pdf
   #:cauchy
   #:cauchy-pdf
   #:rayleigh
   #:rayleigh-pdf
   #:rayleigh-tail
   #:rayleigh-tail-pdf
   #:levy
   #:levy-skew
   #:gamma
   #:gamma-pdf
   #:flat
   #:flat-pdf
   #:lognormal
   #:lognormal-pdf
   #:chi-squared
   #:chi-squared-pdf
   #:fdist
   #:fdist-pdf
   #:tdist
   #:tdist-pdf
   #:beta
   #:beta-pdf
   #:logistic
   #:logistic-pdf
   #:pareto
   #:pareto-pdf
   #:weibull
   #:weibull-pdf
   #:gumbel-1
   #:gumbel-1-pdf
   #:gumbel-2
   #:gumbel-2-pdf
   #:poisson
   #:poisson-pdf
   #:bernoulli
   #:bernoulli-pdf
   #:binomial
   #:binomial-pdf
   #:-binomial
   #:-binomial-pdf
   #:gemoetric
   #:geometric-pdf
   #:logarithmic
   #:logarithmic-pdf)
  ;; volumes.lisp
  (:export
   #:rejection-sample
   #:sphere
   #:half-sphere
   #:normal
   #:disc
   #:box
   #:cylinder
   #:pill
   #:triangle
   #:convex-mesh))
