(in-package #:org.shirakumo.random-sampling)

(defmacro define-sampling-function (name args &body body)
  (let ((result (gensym "RESULT")))
    `(defun ,name (,@args ,@(unless (find '&optional args) '(&optional)) ,result)
       (labels ((sample-1 ()
                  ,@body))
         (declare (dynamic-extent #'sample-1))
         (map-samples ,result #'sample-1)))))

(define-sampling-function rejection-sample (generator predicate)
  (loop for sample = (funcall generator)
        do (when (funcall predicate sample)
             (return sample))))

(define-sampling-function sphere (&optional (radius 1.0))
  (labels ((nonzero ()
             (loop for sample = (random 1.0)
                   when (< 0.0 sample) return sample))
           (randn ()
             (* (sqrt (* -2.0 (log (nonzero))))
                (cos (* 2.0 PI (nonzero))))))
    (let ((x (vec (randn) (randn) (randn))))
      (nv* x (* radius (/ (vlength x)) (expt (random 1.0) 1/3))))))

(define-sampling-function half-sphere (&optional (normal +vy+) (radius 1.0))
  )

(define-sampling-function shell (&optional (radius 1.0))
  )

(define-sampling-function ellipsoid (radius)
  )

(define-sampling-function disc (&optional (radius 1.0) (normal +vy+))
  )

(define-sampling-function box (bsize)
  (vrand 0.0 bsize))

(define-sampling-function cylinder (radius height)
  )

(define-sampling-function pill (radius height)
  )

(define-sampling-function triangle (a b c)
  )

(define-sampling-function convex-mesh (vertices faces)
  )
