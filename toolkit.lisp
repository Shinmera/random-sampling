(in-package #:org.shirakumo.random-sampling)

(defconstant F-PI (float PI 0f0))
(defconstant F-2PI (float (* 2.0 PI) 0f0))
(defconstant F-PI/2 (float (/ PI 2.0) 0f0))
(defconstant F-E (float (exp 1.0) 0f0))

(defun r (&optional (s 1.0))
  (declare (optimize speed))
  (declare (type real s))
  (random-state:random-float random-state:*generator* 0f0 (float s 1f0)))

(define-compiler-macro r (&optional (s 1.0) &environment env)
  (if (constantp s env)
      `(random-state:random-float
        random-state:*generator*
        0f0 (load-time-value (float ,s 0f0)))
      `(random-state:random-float 
        random-state:*generator*
        0f0 (float ,s 0f0))))

(declaim (inline nonzero))
(defun nonzero ()
  (loop for sample = (r)
        when (< 0.0 sample) return sample))

(defun unlist (a)
  (if (listp a) (first a) a))
