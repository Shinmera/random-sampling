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

(define-volume-function surface-normal (sample &optional (normal +vy+) (theta F-PI))
  ;; Generate a half-sphere normal
  (normal 1.0 sample)
  (when (< (vy sample) 0.0)
    (setf (vy sample) (- (vy sample))))
  ;; Constrain the direction further by scaling in the plane
  (let ((scale (/ theta F-PI)))
    (setf (vx sample) (* (vx sample) scale))
    (setf (vz sample) (* (vz sample) scale))
    (nvunit sample))
  ;; Realign the sample
  (align-sample sample normal))

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
        (cylinder radius height +vy+ sample)
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
  (declare (optimize speed (safety 1)))
  (check-type vertices (simple-array single-float (*)))
  (check-type faces (simple-array (unsigned-byte 16) (*)))
  (let ((v0 (vec3)) (v1 (vec3)) (v2 (vec3)) (n (vec3))
        (min (vec3)) (max (vec3)))
    (declare (dynamic-extent v0 v1 v2 n min max))
    (declare (type (simple-array single-float (*)) vertices))
    (declare (type (simple-array (unsigned-byte 16) (*)) faces))
    (declare (type vec3 sample))
    (labels ((v (i v)
               ;; Read out a vertex
               (let ((varr (varr3 v))
                     (i (* 3 i)))
                 (setf (aref varr 0) (aref vertices (+ i 0)))
                 (setf (aref varr 1) (aref vertices (+ i 1)))
                 (setf (aref varr 2) (aref vertices (+ i 2)))))
             (check (p)
               ;; Check if the point is below each triangle plane
               (loop for f from 0 below (length faces) by 3
                     do (v (aref faces (+ f 0)) v0)
                        (v (aref faces (+ f 1)) v1)
                        (v (aref faces (+ f 2)) v2)
                        ;; Compute the normal of the plane
                        (!v- v1 v1 v0)
                        (!v- v2 v2 v0)
                        (!vc n v1 v2)
                        ;; If the dot product is negative, we're below the plane and thus inside
                        (!v- v0 p v0)
                     always (<= (v. v0 n) 0.0))))
      ;; Compute the bounding box
      (v 0 min) (v 0 max)
      (loop for i from 0 below (truncate (length vertices) 3)
            do (v 0 v0)
               (nvmin min v0)
               (nvmax max v0))
      (let* ((bsize (nv* (nv- max min) 0.5))
             (center (nv+ min bsize)))
        ;; Produce samples via rejection sampling
        (loop (!vrand sample center bsize)
              (when (check sample)
                (return sample)))))))
