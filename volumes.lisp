(in-package #:org.shirakumo.random-sampling)

(defun map-volume-samples (result sample-1)
  (declare (optimize speed))
  (declare (type (function (T) T) sample-1))
  (etypecase result
    (null
     (funcall sample-1 (vec3)))
    (vec3
     (funcall sample-1 result))
    (integer
     (let ((array (make-array result)))
       (dotimes (i (length array) array)
         (setf (aref array i) (funcall sample-1 (vec3))))))
    (simple-vector
     (dotimes (i (length result) result)
       (setf (aref result i) (funcall sample-1 (aref result i)))))
    (sequence
     (map-into result sample-1 result))))

(declaim (inline align-sample))
(defun align-sample (sample normal)
  (declare (type vec3 sample normal))
  (if (v/= normal +vy+)
      (let ((quat (quat)))
        (declare (dynamic-extent quat))
        (!qtowards quat +vy+ normal)
        (!q* sample quat sample))
      sample))

(defmacro define-volume-function (name args &body body)
  (let ((result (gensym "RESULT")))
    `(defun ,name (,@(rest args) ,@(unless (find '&optional args) '(&optional)) ,result)
       (labels ((sample-1 (,(first args))
                  ,@body
                  ,(first args)))
         (declare (dynamic-extent #'sample-1))
         (map-volume-samples ,result #'sample-1)))))

(define-volume-function rejection-sample (sample generator predicate)
  (loop do (funcall generator sample)
        until (funcall predicate sample)))

(define-volume-function sphere (sample &optional (radius 1.0))
  ;; This rectified method is faster than rejection sampling a sphere
  (labels ((randn ()
             (* (sqrt (* -2.0 (log (!r 0.0))))
                (cos (* F-2PI (!r 0.0))))))
    (vsetf sample (randn) (randn) (randn))
    (nv* sample (* (/ (vlength sample)) (expt (r) 1/3)))
    (nv* sample radius)))

(define-volume-function half-sphere (sample &optional (normal +vy+) (radius 1.0))
  ;; We generate a sphere and remap it to +Y if it's -Y, then rotate.
  (sphere radius sample)
  (when (< (vy sample) 0.0)
    (setf (vy sample) (- (vy sample))))
  (align-sample sample normal))

(define-volume-function normal (sample &optional (radius 1.0))
  ;; Rectified polar coordinates for the surface with a fixed radius
  (let* ((lat (- (acos (1- (* 2 (r)))) F-PI/2))
         (lng (* F-2PI (r)))
         (clat (cos lat)) (clng (cos lng))
         (slat (sin lat)) (slng (sin lng)))
    (vsetf sample (* clat clng) (* clat slng) (* slat))
    (nv* sample radius)))

(define-volume-function disc (sample &optional (radius 1.0) (normal +vy+))
  ;; Radius needs to be rectified to avoid clustering at the center
  (let ((r (* radius (sqrt (r))))
        (phi (r F-2PI)))
    (vsetf sample (* r (cos phi)) 0 (* r (sin phi)))
    (align-sample sample normal)))

(define-volume-function box (sample bsize)
  ;; All axes are independent, so we just randomise a vec3
  (!vrand sample 0.0 bsize))

(define-volume-function cylinder (sample radius height &optional (normal +vy+))
  ;; The height is independent, so we can just move the disc
  (disc radius +vy+ sample)
  (setf (vy sample) (1- (r (* 2.0 height))))
  (align-sample sample normal))

(define-volume-function pill (sample radius height &optional (normal +vy+))
  ;; Pick randomly between cylinder and sphere based on volume
  (let* ((r2 (* radius radius))
         (cylinder-volume (* F-PI height r2))
         (sphere-volume (* 4/3 F-PI r2)))
    (if (< (r (+ cylinder-volume sphere-volume)) cylinder-volume)
        (cylinder radius height sample)
        (let ((sample (sphere radius sample)))
          ;; Offset sphere to corresponding cap
          (if (<= 0.0 (vy sample))
              (incf (vy sample) height)
              (decf (vy sample) height))))
    (align-sample sample normal)))

(define-volume-function triangle (sample p0 p1 p2)
  ;; Barycentric coordinate interpolation
  (let ((f (r))
        (g (r)))
    (when (< 1.0 (+ f g))
      (setf f (- 1.0 f) g (- 1.0 g)))
    (let ((ba (v- p1 p0))
          (ca (v- p2 p0)))
      (declare (dynamic-extent ba ca))
      (nv+* (v+* (v<- sample p0) ba f) ca g))))

(define-volume-function convex-mesh (sample vertices faces)
  )
