(in-package #:org.shirakumo.random-sampling)

(defun map-samples (result sample-1)
  (declare (optimize speed))
  (declare (type (function () single-float) sample-1))
  (labels ((sample (array)
             (declare (type (simple-array single-float (*))))
             (dotimes (i (length array) array)
               (setf (aref array i) (funcall sample-1)))))
    (etypecase result
      (null
       (funcall sample-1))
      (integer
       (sample (make-array result :element-type 'single-float)))
      ((simple-array single-float (*))
       (sample result))
      (sequence
       (map-into result sample-1)))))
