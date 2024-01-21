(in-package #:org.shirakumo.random-sampling)

;; distributions.lisp
(docs:define-docs
  (function map-samples
    "Gathers samples into RESULT, drawing them from SAMPLE-1.

RESULT may be one of the following:

  NULL      --- A single single-float is sampled and returned
  INTEGER   --- An array of result size is sampled and returned
  SEQUENCE  --- Each element of the sequence is set to a fresh sample

SAMPLE-1 must be a function of zero arguments and must return the
freshly drawn sample as a single-float.

This function is implicitly used by every distribution sampling
function. The optional RESULT argument of those functions corresponds
to the above.")
  
  (function pdf
    "Returns the probability density function for the given distribution.

The distribution must be given as a name. If no PDF exists, and errorp
is true, an error is signalled.

Every PDF takes the same arguments as its corresponding distribution
function, plus an additional first argument, which is the sample for
which to evaluate the PDF.")

  (function gaussian
    "Draws a sample from the Gaussian distribution.

See GAUSSIAN-TAIL
See GAUSSIAN-PDF
See MAP-SAMPLES")
  
  (function gaussian-pdf
    "Computes the probability density of the given sample X under the Gaussian distribution.

See PDF
See GAUSSIAN")
  
  (function gaussian-tail
    "Draws a sample from the Gaussian-tail distribution.

See GAUSSIAN
See GAUSSIAN-TAIL-PDF
See MAP-SAMPLES")
  
  (function gaussian-tail-pdf
    "Computes the probability density of the given sample X under the Gaussian-tail distribution.

See PDF
See GAUSSIAN-TAIL")
  
  (function exponential
    "Draws a sample from the exponential distribution.

See EXPONENTIAL-PDF
See MAP-SAMPLES")
  
  (function exponential-pdf
    "Computes the probability density of the given sample X under the exponential distribution.

See PDF
See EXPONENTIAL")
  
  (function laplace
    "Draws a sample from the Laplace distribution.

See LAPLACE-PDF
See MAP-SAMPLES")
  
  (function laplace-pdf
    "Computes the probability density of the given sample X under the Laplace distribution.

See PDF
See LAPLACE")
  
  (function cauchy
    "Draws a sample from the Cauchy distribution.

See CAUCHY-PDF
See MAP-SAMPLES")
  
  (function cauchy-pdf
    "Computes the probability density of the given sample X under the Cauchy distribution.

See PDF
See CAUCHY")

  (function rayleigh
    "Draws a sample from the Rayleigh distribution.

See RAYLEIGH-TAIL
See RAYLEIGH-PDF
See MAP-SAMPLES")
  
  (function rayleigh-pdf
    "Computes the probability density of the given sample X under the Rayleigh distribution.

See PDF
See RAYLEIGH")

  (function rayleigh-tail
    "Draws a sample from the Rayleigh-tail distribution.

See RAYLEIGH
See RAYLEIGH-TAIL-PDF
See MAP-SAMPLES")
  
  (function rayleigh-tail-pdf
    "Computes the probability density of the given sample X under the Rayleigh-tail distribution.

See PDF
See RAYLEIGH-TAIL")

  (function levy
    "Draws a sample from the Levy distribution.

See LEVY-SKEW
See MAP-SAMPLES")
  
  (function levy-skew
    "Draws a sample from the Levy-skew distribution.

See LEVY
See MAP-SAMPLES")
  
  (function gamma
    "Draws a sample from the gamma distribution.

See GAMMA-PDF
See MAP-SAMPLES")

  (function gamma-pdf
    "Computes the probability density of the given sample X under the gamma distribution.

See PDF
See GAMMA")

  (function flat
    "Draws a sample from the flat distribution.

See FLAT-PDF
See MAP-SAMPLES")
  
  (function flat-pdf
    "Computes the probability density of the given sample X under the flat distribution.

See PDF
See FLAT")

  (function lognormal
    "Draws a sample from the lognormal distribution.

See LOGNORMAL-PDF
See MAP-SAMPLES")
  
  (function lognormal-pdf
    "Computes the probability density of the given sample X under the lognormal distribution.

See PDF
See LOGNORMAL")

  (function chi-squared
    "Draws a sample from the chi-squared distribution.

See CHI-SQUARED-PDF
See MAP-SAMPLES")
  
  (function chi-squared-pdf
    "Computes the probability density of the given sample X under the chi-squared distribution.

See PDF
See CHI-SQUARED")

  (function fdist
    "Draws a sample from the F distribution.

See FDIST-PDF
See MAP-SAMPLES")
  
  (function fdist-pdf
    "Computes the probability density of the given sample X under the F distribution.

See PDF
See FDIST")

  (function tdist
    "Draws a sample from the T distribution.

See TDIST-PDF
See MAP-SAMPLES")
  
  (function tdist-pdf
    "Computes the probability density of the given sample X under the T distribution.

See PDF
See TDIST")

  (function beta
    "Draws a sample from the beta distribution.

See BETA-PDF
See MAP-SAMPLES")
  
  (function beta-pdf
    "Computes the probability density of the given sample X under the beta distribution.

See PDF
See BETA")

  (function logistic
    "Draws a sample from the logistic distribution.

See LOGISTIC-PDF
See MAP-SAMPLES")
  
  (function logistic-pdf
    "Computes the probability density of the given sample X under the logistic distribution.

See PDF
See LOGISTIC")

  (function pareto
    "Draws a sample from the Pareto distribution.

See PARETO-PDF
See MAP-SAMPLES")
  
  (function pareto-pdf
    "Computes the probability density of the given sample X under the Pareto distribution.

See PDF
See PARETO")

  (function weibull
    "Draws a sample from the Weibull distribution.

See WEIBULL-PDF
See MAP-SAMPLES")
  
  (function weibull-pdf
    "Computes the probability density of the given sample X under the Weibull distribution.

See PDF
See WEIBULL")

  (function gumbel-1
    "Draws a sample from the Gumbel-1 distribution.

See GUMBEL-1-PDF
See MAP-SAMPLES")
  
  (function gumbel-1-pdf
    "Computes the probability density of the given sample X under the Gumbel-1 distribution.

See PDF
See GUMBEL-1")

  (function gumbel-2
    "Draws a sample from the Gumbel-2 distribution.

See GUMBEL-2-PDF
See MAP-SAMPLES")
  
  (function gumbel-2-pdf
    "Computes the probability density of the given sample X under the Gumbel-2 distribution.

See PDF
See GUMBEL-2")

  (function poisson
    "Draws a sample from the Poisson distribution.

See POISSON-PDF
See MAP-SAMPLES")
  
  (function poisson-pdf
    "Computes the probability density of the given sample X under the Poisson distribution.

See PDF
See POISSON")

  (function bernoulli
    "Draws a sample from the Bernoulli distribution.

See BERNOULLI-PDF
See MAP-SAMPLES")
  
  (function bernoulli-pdf
    "Computes the probability density of the given sample X under the Bernoulli distribution.

See PDF
See BERNOULLI")

  (function binomial
    "Draws a sample from the binomial distribution.

See -BINOMIAL
See BINOMIAL-PDF
See MAP-SAMPLES")
  
  (function binomial-pdf
    "Computes the probability density of the given sample X under the binomial distribution.

See PDF
See BINOMIAL")

  (function -binomial
    "Draws a sample from the negative binomial distribution.

See BINOMIAL
See -BINOMIAL-PDF
See MAP-SAMPLES")
  
  (function -binomial-pdf
    "Computes the probability density of the given sample X under the -binomial distribution.

See PDF
See -BINOMIAL")

  (function geometric
    "Draws a sample from the geometric distribution.

See GEOMETRIC-PDF
See MAP-SAMPLES")
  
  (function geometric-pdf
    "Computes the probability density of the given sample X under the geometric distribution.

See PDF
See GEOMETRIC")

  (function logarithmic
    "Draws a sample from the logarithmic distribution.

See LOGARITHMIC-PDF
See MAP-SAMPLES")
  
  (function logarithmic-pdf
    "Computes the probability density of the given sample X under the logarithmic distribution.

See PDF
See LOGARITHMIC"))

;; volumes.lisp
(docs:define-docs
  (function map-volume-samples
    "Gathers samples into RESULT, drawing them from SAMPLE-1.

RESULT may be one of the following:

  NULL      --- A fresh VEC3 is sampled and returned
  VEC3      --- The result is sampled and returned
  INTEGER   --- An array of result size is sampled and returned
  SEQUENCE  --- Each element of result is sampled and result is
                returned. Each element of result must be a VEC3

SAMPLE-1 must be a function of one argument, a VEC3, and must return
said VEC3 after performing its sampling operation to fill the vector's
elements.

This function is implicitly used by every volume sampling
function. The optional RESULT argument of those functions corresponds
to the above.")
  
  (function rejection-sample
    "Perform rejection sampling using GENERATOR to draw samples and PREDICATE to determine whether to keep the sample.

GENERATOR must be a function of one argument, the VEC3 to sample.
PREDICATE must be a function of one argument, the VEC3 to examine, and
must return true if the sample is suitable.

See MAP-VOLUME-SAMPLES")
  
  (function sphere
    "Samples a uniform sphere.

RADIUS may either be a real of the radius, or a VEC3 to generate an
ellipsoid.

See MAP-VOLUME-SAMPLES")
  
  (function half-sphere
    "Samples a uniform half-sphere.

RADIUS may either be a real of the radius, or a VEC3 to generate an
ellipsoid. NORMAL may be a normalised vector pointing in the UP
direction of the half-sphere.

See MAP-VOLUME-SAMPLES")
  
  (function normal
    "Samples a uniform normal vector.

This is equivalent to sampling the surface of a sphere.

RADIUS may either be a real of the radius, or a VEC3 to generate an
ellipsoid.

See MAP-VOLUME-SAMPLES")
  
  (function disc
    "Samples a uniform disc.

NORMAL may be a normalised vector pointing in the UP direction of the
disc's plane.

See MAP-VOLUME-SAMPLES")
  
  (function box
    "Samples a uniform box.

BSIZE should be the half-size of the box in each direction, with the
box being centered at 0,0,0.

See MAP-VOLUME-SAMPLES")
  
  (function cylinder
    "Samples a uniform cylinder.

HEIGHT is the half-height of the cylinder with the cylinder's center
always at 0,0,0.
NORMAL may be a normalised vector pointing in the UP direction of the
cylinder.

See MAP-VOLUME-SAMPLES")
  
  (function pill
    "Samples a uniform pill.

A pill is a cylinder with its caps being spheres.

HEIGHT is the half-height of the pill cylinder with the pill's center
always at 0,0,0. If the height is zero, this is equivalent to a
sphere.
NORMAL may be a normalised vector pointing in the UP direction of the
cylinder.

See MAP-VOLUME-SAMPLES")
  
  (function triangle
    "Samples a uniform triangle.

P0, P1, P2 designate the corners of the triangle.

See MAP-VOLUME-SAMPLES")
  
  (function convex-mesh
    "Samples a uniform convex mesh.

VERTICES must be a (SIMPLE-ARRAY SINGLE-FLOAT (*)) that contains the
packed representation of the vertex coordinates.
FACES must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (*)) that contains
the packed indices of the vertices that compose the faces.
Both arrays must have a length that is a multiple of 3.

The consequences are undefined if the mesh described by VERTICES and
FACES is not convex and manifold.

See MAP-VOLUME-SAMPLES"))
