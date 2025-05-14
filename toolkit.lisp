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

(declaim (inline !r))
(defun !r (not)
  (loop for sample = (r)
        when (/= not sample) return sample))

(defun unlist (a)
  (if (listp a) (first a) a))

(defun plot-pdf (pdf &key (width (or *print-right-margin* 80)) (height 15)
                          (left 0) (right 1) (bottom 0) (top 1))
  (let ((plot-width (- width 2))
        (plot-height (- height 2)))
    (format T "~&~5,2@f~v@{▁~} ~%" top (- plot-width 4) 0)
    (loop with samples = (loop for x from 0 below plot-width
                               collect (funcall pdf (float (+ left (* (/ x (1- plot-width)) (- right left))) 0f0)))
          for i downfrom height to 0
          for threshold = (/ i plot-height)
          do (format T "▕")
             (dolist (sample samples)
               (format T "~[ ~;▁~;▂~;▃~;▄~;▅~;▆~;▇~;█~]"
                       (round (* 8 (min 1.0 (max 0.0 (* plot-height (- sample threshold))))))))
             (format T "▏~%"))
    (format T "~5,2@f/~5,2@f~v@{▔~}~5,2@f" bottom left (- plot-width 14) right)))
