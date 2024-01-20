(in-package #:org.shirakumo.random-sampling)
;;;; Based on
;;;;   https://github.com/stevengj/nlopt/blob/master/src/util/sobolseq.c
;;;;   https://github.com/stevengj/nlopt/blob/master/src/util/soboldata.h

(alexandria:define-constant sobol-a
  (make-array 3 :element-type '(unsigned-byte 32) :initial-contents
              '(3 7 11))
  :test 'equalp)

(alexandria:define-constant sobol-init
    (make-array '(13 4) :element-type '(unsigned-byte 32) :initial-contents
                '((1 1 1 1) (0 1 3 1) (0 0 7 5) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)
                  (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
  :test 'equalp)

(defun rightzero32 (n)
  (declare (optimize speed (safety 0)))
  (declare (type (unsigned-byte 32) n))
  (loop for i from 0 below 32
        do (unless (logbitp i n)
             (return i))))

(macrolet ((m (j i)
             `(aref mdata (+ ,i (aref m ,j)))))
  (random-state:define-generator sobol-2 32 (random-state:stateful-generator)
      ((mdata (make-array 64 :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (64)))
       (m (make-array 32 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (32)))
       (x (make-array 2 :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (2)))
       (b (make-array 2 :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (2)))
       (n 0 :type (unsigned-byte 32)))
    (:reseed
     (dotimes (j 32)
       (setf (aref m j) (* j 2))
       (setf (m j 0) 1))
     (loop for i from 1 below 2
           for a = (aref sobol-a (1- i))
           for d = 0
           for k = 0
           do (loop while (< 0 a)
                    do (incf d)
                       (setf a (ash a -1)))
              (decf d)
              (dotimes (j d)
                (setf (m j i) (aref sobol-init j (1- i))))
              (loop for j from d below 32
                    for a = (aref sobol-a (1- i))
                    do (setf (m j i) (m (- j d) i))
                       (dotimes (k d)
                         (setf (m j i) (logxor (m j i) (ash (* (logand a 1) (m (+ (- j d) k) i))
                                                            (- d k))))
                         (setf a (ash a -1)))))
     (dotimes (i 2)
       (setf (aref x i) 0)
       (setf (aref b i) 0)))
    (:next
     (assert (< n 4294967295))
     (let ((c (rightzero32 (incf n))))
       (flet ((dim (i)
                (let ((b_ (aref b i)))
                  (cond ((<= c b_)
                         (setf (aref x i) (logxor (aref x i) (ash (m c i) (- b_ c))))
                         (float (/ (aref x i) (ash 1 (1+ b_)))))
                        (T
                         (setf (aref x i) (logxor (ash (aref x i) (- c b_)) (m c i)))
                         (setf (aref b i) c)
                         (float (/ (aref x i) (ash 1 (1+ c)))))))))
         (vec (dim 0) (dim 1))))))

  (random-state:define-generator sobol-3 32 (random-state:stateful-generator)
      ((mdata (make-array 96 :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (96)))
       (m (make-array 32 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (32)))
       (x (make-array 3 :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (3)))
       (b (make-array 3 :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (3)))
       (n 0 :type (unsigned-byte 32)))
    (:reseed
     (dotimes (j 32)
       (setf (aref m j) (* j 2))
       (setf (m j 0) 1))
     (loop for i from 1 below 3
           for a = (aref sobol-a (1- i))
           for d = 0
           for k = 0
           do (loop while (< 0 a)
                    do (incf d)
                       (setf a (ash a -1)))
              (decf d)
              (dotimes (j d)
                (setf (m j i) (aref sobol-init j (1- i))))
              (loop for j from d below 32
                    for a = (aref sobol-a (1- i))
                    do (setf (m j i) (m (- j d) i))
                       (dotimes (k d)
                         (setf (m j i) (logxor (m j i) (ash (* (logand a 1) (m (+ (- j d) k) i))
                                                            (- d k))))
                         (setf a (ash a -1)))))
     (dotimes (i 3)
       (setf (aref x i) 0)
       (setf (aref b i) 0)))
    (:next
     (assert (< n 4294967295))
     (let ((c (rightzero32 (incf n))))
       (flet ((dim (i)
                (let ((b_ (aref b i)))
                  (cond ((<= c b_)
                         (setf (aref x i) (logxor (aref x i) (ash (m c i) (- b_ c))))
                         (float (/ (aref x i) (ash 1 (1+ b_)))))
                        (T
                         (setf (aref x i) (logxor (ash (aref x i) (- c b_)) (m c i)))
                         (setf (aref b i) c)
                         (float (/ (aref x i) (ash 1 (1+ c)))))))))
         (vec (dim 0) (dim 1) (dim 1)))))))
