(in-package #:org.shirakumo.random-sampling)

;; distributions.lisp
(docs:define-docs
  (function pdf
    "Returns the probability density function for the given distribution.

The distribution must be given as a name. If no PDF exists, and errorp
is true, an error is signalled.")

  (function gaussian
    "")
  
 (function gaussian-pdf
   "")
 
 (function gaussian-tail
   "")
 
 (function gaussian-tail-pdf
   "")
 
 (function exponential
   "")
 
 (function exponential-pdf
   "")
 
 (function laplace
   "")
 
 (function laplace-pdf
   "")
 
 (function cauchy
   "")
 
 (function cauchy-pdf
   "")
 
 (function rayleigh
   "")
 
 (function rayleigh-pdf
   "")
 
 (function rayleigh-tail
   "")
 
 (function rayleigh-tail-pdf
   "")
 
 (function levy
   "")
 
 (function levy-skew
   "")
 
 (function gamma
   "")
 
 (function gamma-pdf
   "")
 
 (function flat
   "")
 
 (function flat-pdf
   "")
 
 (function lognormal
   "")
 
 (function lognormal-pdf
   "")
 
 (function chi-squared
   "")
 
 (function chi-squared-pdf
   "")
 
 (function fdist
   "")
 
 (function fdist-pdf
   "")
 
 (function tdist
   "")
 
 (function tdist-pdf
   "")
 
 (function beta
   "")
 
 (function beta-pdf
   "")
 
 (function logistic
   "")
 
 (function logistic-pdf
   "")
 
 (function pareto
   "")
 
 (function pareto-pdf
   "")
 
 (function weibull
   "")
 
 (function weibull-pdf
   "")
 
 (function gumbel-1
   "")
 
 (function gumbel-1-pdf
   "")
 
 (function gumbel-2
   "")
 
 (function gumbel-2-pdf
   "")
 
 (function poisson
   "")
 
 (function poisson-pdf
   "")
 
 (function bernoulli
   "")
 
 (function bernoulli-pdf
   "")
 
 (function binomial
   "")
 
 (function binomial-pdf
   "")
 
 (function -binomial
   "")
 
 (function -binomial-pdf
   "")
 
 (function gemoetric
   "")
 
 (function geometric-pdf
   "")
 
 (function logarithmic
   "")
 
 (function logarithmic-pdf
   ""))

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
