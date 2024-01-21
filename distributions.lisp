(in-package #:org.shirakumo.random-sampling)

(defun map-samples (result sample-1)
  (declare (optimize speed))
  (declare (type (function () single-float) sample-1))
  (etypecase result
    (null
     (funcall sample-1))
    (integer
     (let ((array (make-array result :element-type 'single-float)))
       (dotimes (i (length array) array)
         (setf (aref array i) (funcall sample-1)))))
    ((simple-array single-float (*))
     (dotimes (i (length result) result)
       (setf (aref result i) (funcall sample-1))))
    (sequence
     (map-into result sample-1))))

(defmacro define-distribution-function (name args &body body)
  (let ((result (gensym "RESULT"))
        (sample-1 (intern (format NIL "~a1" name))))
    `(progn
       (declaim (ftype (function ,(loop for arg in args collect (if (find arg lambda-list-keywords) arg 'real)) single-float) ,sample-1))
       (defun ,sample-1 ,args
         ,@body)
       
       (defun ,name (,@args ,@(unless (find '&optional args) '(&optional)) ,result)
         (flet ((,sample-1 ()
                  ,@body))
           (declare (dynamic-extent #',sample-1))
           (map-samples ,result #',sample-1)))

       (define-compiler-macro ,name (&whole whole ,@args ,@(unless (find '&optional args) '(&optional)) ,result)
         (if ,result
             whole
             (list ',sample-1 ,@(loop for arg in args unless (find arg lambda-list-keywords) collect (unlist arg))))))))

(defmacro define-probability-density-function (name args &body body)
  (let ((pdf (intern (format NIL "~a-~a" name 'pdf))))
    `(progn
       (declaim (ftype (function (single-float ,@(loop for arg in (rest args) collect (if (find arg lambda-list-keywords) arg 'real))) single-float) ,pdf))
       (defun ,pdf (,@args)
         (declare (type single-float ,(first args)))
         ,@body)

       (setf (get ',name 'pdf) #',pdf))))

(defun pdf (distribution &optional (errorp T))
  (or (get distribution 'pdf)
      (when errorp (error "No PDF for ~s is known." distribution))))

(defmacro %poly (&rest facs)
  (if facs
      (destructuring-bind (x &optional s &rest facs) facs
        (if s
            `(+ ,x (* ,s (%poly ,@facs)))
            x))
      1.0))

;; See Numerical Recipes, 6.2
(defun erf (z)
  ;; FIXME: This behaves pretty bad when Z is close to zero, but whatever.
  (let* ((x (/ (1+ (* 0.5 (abs z)))))
         (p (%poly +1.26551223 x 1.00002368 x  0.37409196 x 0.09678418 x
                   -0.18628806 x 0.27886807 x -1.13520398 x 1.48851587 x
                   -0.82215223 x 0.17087277)))
    (abs (- 1 (* x (exp (- (* (- z) z) p)))))))

(defun erfc (z)
  (- 1 (erf z)))

(defun lnchoose (n m)
  (assert (< n m))
  (if (or (= m n) (= m 0))
      0.0
      (let ((m (if (< n (* m 2)) (- n m) m)))
        (- (lnfact n) (lnfact m) (lnfact (- n m))))))

(defun lnfact (n)
  ;; TODO: can be optimised via tabulation
  (lngamma (1+ n)))

(defun lngamma (x)
  (assert (/= 0.0 x))
  ;; TODO: accuracy can be improved with other methods
  (let ((factors #(0.99999999999980993227684700473478d0
                   676.520368121885098567009190444019d0
                   -1259.13921672240287047156078755283d0
                   771.3234287776530788486528258894d0
                   -176.61502916214059906584551354d0
                   12.507343278686904814458936853d0
                   -0.13857109526572011689554707d0
                   9.984369578019570859563d-6
                   1.50563273514931155834d-7)))
    (let ((ag 0.99999999999980993227684700473478d0))
      (loop for i from 1 below 8
            do (incf ag (/ (aref factors i) (+ x i))))
      (float (+ (* (+ x 0.5) (log (/ (+ x 7.5) F-E)))
                (- (+ (log (sqrt F-2PI)) (log ag)) 7.0))
             0f0))))

(define-distribution-function gaussian (&optional (sigma 1.0))
  (loop for x = (1- (r 2.0))
        for y = (1- (r 2.0))
        for distance = (+ (* x x) (* y y))
        do (when (and (< 0.0 distance) (<= distance 1.0))
             (return (* sigma y (sqrt (/ (* -2.0 (log distance)) distance)))))))

(define-probability-density-function gaussian (x &optional (sigma 1.0))
  (let ((u (/ x (abs sigma))))
    (* (/ (* (sqrt F-2PI) (abs sigma))) (exp (* 0.5 u (- u))))))

(define-distribution-function gaussian-tail (a &optional (sigma 1.0))
  (let ((s (/ a sigma)))
    (* sigma
       (cond ((< s 1.0)
              (loop for sample = (gaussian sigma)
                    do (when (<= s sample) (return sample))))
             (T
              (loop for u = (r)
                    for v = (!r 0.0)
                    for sample = (sqrt (- (* s s) (* 2 (log v))))
                    do (when (<= (* sample u) s) (return sample))))))))

(define-probability-density-function gaussian-tail (x a &optional (sigma 1.0))
  (if (< x a)
      0.0
      (let ((u (/ x sigma))
            (n (* 0.5 (erfc (/ a (sqrt 2.0) sigma)))))
        (* (/ (* n (sqrt F-2PI) (abs sigma))) (exp (* 0.5 u (- u)))))))

(define-distribution-function exponential (mu)
  (* (- mu) (log (1+ (- (r))))))

(define-probability-density-function exponential (x mu)
  (if (< x 0) 0.0 (/ (exp (/ (- x) mu)) mu)))

(define-distribution-function laplace (a)
  (loop for u = (1- (r 2.0))
        do (cond ((< u 0)
                  (* (+ a) (log u)))
                 ((< 0 u)
                  (* (- a) (log u))))))

(define-probability-density-function laplace (x a)
  (* (/ (* 2 a)) (exp (/ (- (abs x)) a))))

;; TODO: exponential power

(define-distribution-function cauchy (a)
  (* a (tan (* F-PI (!r 0.5)))))

(define-probability-density-function cauchy (x a)
  (/ (/ (* F-PI a)) (1+ (expt (/ x a) 2))))

(define-distribution-function rayleigh (sigma)
  (* sigma (sqrt (* -2.0 (log (r))))))

(define-probability-density-function rayleigh (x sigma)
  (if (< x 0)
      0.0
      (let ((u (/ x sigma)))
        (* (/ u sigma) (exp (/ (* (- u) u) 2.0))))))

(define-distribution-function rayleigh-tail (a sigma)
  (sqrt (- (* a a) (* 2.0 sigma sigma (log (r))))))

(define-probability-density-function rayleigh-tail (x a sigma)
  (if (< x a)
      0.0
      (let ((u (/ x sigma))
            (v (/ a sigma)))
        (* (/ u sigma) (exp (* (+ v u) (- v u) 0.5))))))

;; TODO: landau

(define-distribution-function levy (c alpha)
  (let ((u (* F-PI (- (r) 0.5))))
    (if (= 1 alpha)
        (* c (tan u))
        (loop for v = (exponential 1.0)
              do (when (/= v 0.0)
                   (if (= 2 alpha)
                       (* c 2.0 (sin u) (sqrt v))
                       (* c
                          (/ (sin (* alpha u)) (expt (cos u) (/ alpha)))
                          (expt (/ (cos (* (- 1 alpha) u)) v) (/ (- 1 alpha) alpha)))))))))

(define-distribution-function levy-skew (c alpha beta)
  (if (= 0 beta)
      (levy c alpha)
      (let ((v (* F-PI (- (r) 0.5)))
            (w (!r 0.0)))
        (if (= 1 alpha)
            (let ((tmp (+ F-PI/2 (* beta v))))
              (* c (/ (+ (- (* (tan v) tmp)
                            (* beta (log (/ (* F-PI/2 w (cos v)) tmp))))
                         (* beta (log c))))))
            (let* ((tt (* beta (tan (* alpha F-PI/2))))
                   (b (/ (atan tt) alpha))
                   (s (expt (1+ (* tt tt)) (/ (* 2 alpha)))))
              (* c s (/ (sin (* alpha (+ v b))) (expt (cos v) (/ alpha)))
                 (expt (/ (cos (- v (* alpha (+ v b)))) w) (/ (- 1 alpha) alpha))))))))

;; TODO: gauss ziggurat

(defun gamma-large (a)
  (let ((sqa (1- (* 2.0 a)))
        (x 1.0) (y 1.0))
    (loop do (loop do (setf y (tan (* F-PI (r)))
                            x (+ (* sqa y) (1- a)))
                   while (<= x 0))
          while (< (* (1+ (* y y)) (exp (- (* (1- a) (log (/ x (1- a)))) (* sqa y)))) (r))
          finally (return x))))

(defun gamma-int (a)
  (if (< a 12)
      (- (log (loop repeat a
                    for prod = 1 then (* prod (r))
                    finally (return prod))))
      (gamma-large (float a 0f0))))

(define-distribution-function gamma (a &optional (b 1.0))
  (let ((na (floor a)))
    (labels ((frac (a)
               (if (= 0 a)
                   0.0
                   (loop with p = (/ F-E (+ a F-E))
                         for u = (r)
                         for v = (!r 0.0)
                         for x = (if (< u p)
                                     (exp (* (/ a) (log v)))
                                     (- 1 (log v)))
                         for q = (if (< u p)
                                     (exp (- x))
                                     (exp (* (1- a) (log x))))
                         while (<= q (r))
                         finally (return x)))))
      (* b
         (cond ((<= MOST-POSITIVE-FIXNUM a)
                (+ (gamma-large na) (frac (- a na))))
               ((= na a)
                (gamma-int na))
               ((= na 0)
                (frac a))
               (T
                (+ (gamma-int na) (frac (- a na)))))))))

(define-probability-density-function gamma (x a &optional (b 1.0))
  (/ (cond ((< x 0)
            0.0)
           ((= x 0)
            (if (= a 1)
                1.0
                0.0))
           ((= a 1)
            (exp (/ (- x) b)))
           (T
            (exp (- (* (1- a) (log (/ x b))) (/ x b) (lngamma a)))))
     b))

(define-distribution-function flat (a b)
  (let ((u (r)))
    (+ (* a (- 1 u)) (* b u))))

(define-probability-density-function flat (x a b)
  (if (and (<= a x) (< x b))
      (/ (- b a))
      0.0))

(define-distribution-function lognormal (zeta sigma)
  (loop for x = (1- (r 2.0))
        for y = (1- (r 2.0))
        for distance = (+ (* x x) (* y y))
        do (when (and (< 0.0 distance) (<= distance 1.0))
             (let ((normal (* x (sqrt (/ (* -2.0 (log distance)) distance)))))
               (return (exp (+ zeta (* sigma normal))))))))

(define-probability-density-function lognormal (x zeta sigma)
  (if (<= x 0)
      0.0
      (/ (exp (/ (expt (/ (- (log x) zeta) sigma) 2) -2.0))
         (* x (abs sigma) (sqrt F-2PI)))))

(define-distribution-function chi-squared (nu)
  (* 2.0 (gamma (* 0.5 nu))))

(define-probability-density-function chi-squared (x nu)
  (/ (cond ((< x 0)
            0.0)
           ((= nu 2)
            (exp (/ x -2.0)))
           (T
            (exp (- (* (1- (/ nu 2.0)) (log (/ x 2.0))) (/ x 2.0) (lngamma (/ nu 2.0))))))
     2.0))

(define-distribution-function fdist (nu-1 nu-2)
  (/ (* nu-2 (gamma (/ nu-1 2.0) 2.0))
     (* nu-1 (gamma (/ nu-2 2.0) 2.0))))

(define-probability-density-function fdist (x nu-1 nu-2)
  (if (< x 0)
      0.0
      (let* ((lglg (+ (* (/ nu-1 2.0) (log nu-1)) (* (/ nu-2 2.0) (log nu-2))))
             (lg12 (lngamma (/ (+ nu-1 nu-2) 2.0)))
             (lg1 (lngamma (/ nu-1 2.0)))
             (lg2 (lngamma (/ nu-2 2.0))))
        (exp (+ lglg lg12 (- lg1) (- lg2) (* (1- (/ nu-1 2.0)) (log x))
                (- (* (/ (+ nu-1 nu-2) 2.0) (log (+ nu-2 (* nu-1 x))))))))))

(define-distribution-function tdist (nu)
  (if (<= nu 2)
      (/ (gaussian) (sqrt (/ (chi-squared nu) nu)))
      (loop for y1 = (gaussian)
            for y2 = (exponential (/ (1- (/ nu 2.0))))
            for z = (/ (* y1 y1) (- nu 2))
            do (unless (or (< (1- z) 0) (< (1- z) (exp (- (- y2) z))))
                 (return (/ y1 (sqrt (* (- 1 (/ 2.0 nu)) (- 1 z)))))))))

(define-probability-density-function tdist (x nu)
  (* (/ (exp (- (lngamma (/ nu 2.0)) (lngamma (/ (1+ nu) 2.0))))
        (sqrt (* F-PI 2.0)))
     (expt (1+ (/ (* x x) nu)) (/ (1+ nu) -2.0))))

(define-distribution-function beta (a b)
  (if (and (<= a 1.0) (<= b 1.0))
      (loop for u = (!r 0.0)
            for v = (!r 0.0)
            for x = (expt u (/ a))
            for y = (expt v (/ a))
            do (when (<= (+ x y) 1.0)
                 (return
                   (if (< 0 (+ x y))
                       (/ x (+ x y))
                       (let* ((logx (/ (log u) a))
                              (logy (/ (log v) b))
                              (logm (max logx logy)))
                         (decf logx logm)
                         (decf logy logm)
                         (exp (- logx (log (+ (exp logx) (exp logy))))))))))
      (let ((x (gamma a)))
        (/ x (+ x (gamma b))))))

(define-probability-density-function beta (x a b)
  (if (or (< x 0) (< 1 x))
      0.0
      (let ((gab (lngamma (+ a b)))
            (ga (lngamma a))
            (gb (lngamma b)))
        (if (or (= x 0) (= x 1))
            (if (and (< 1 a) (< 1 b))
                0.0
                (* (exp (- gab ga gb))
                   (expt x (1- a))
                   (expt (- 1 x) (1- b))))
            (exp (+ (- gab ga gb) (* (log x) (1- a)) (* (log (- 1 x)) (1- b))))))))

(define-distribution-function logistic (a)
  (let ((x (r)))
    (* a (log (/ x (- 1 x))))))

(define-probability-density-function logistic (x a)
  (let ((u (exp (/ (- (abs x)) a))))
    (/ u (* (abs a) (1+ u) (1+ u)))))

(define-distribution-function pareto (a b)
  (* b (expt (r) (/ -1 a))))

(define-probability-density-function pareto (x a b)
  (if (<= b x)
      (/ a b (expt (/ x b) (1+ a)))
      0.0))

(define-distribution-function weibull (a b)
  (* a (expt (- (log (r))) (/ b))))

(define-probability-density-function weibull (x a b)
  (cond ((< x 0)
         0.0)
        ((= x 0)
         (if (= b 1)
             (/ a)
             0.0))
        ((= b 1)
         (/ (exp (/ (- x) a)) a))
        (T
         (* (/ b a) (exp (- (* (log (/ x a)) (1- b)) (expt (/ x a) b)))))))

(define-distribution-function gumbel-1 (a b)
  (/ (- (log b) (log (- (log (r))))) a))

(define-probability-density-function gumbel-1 (x a b)
  (* a b (exp (- (+ (* b (exp (* x (- a)))) (* a x))))))

(define-distribution-function gumbel-2 (a b)
  (expt (/ (- b) (log (r))) (/ a)))

(define-probability-density-function gumbel-2 (x a b)
  (if (<= x 0)
      0.0
      (* b a (expt x (- (1+ a))) (exp (* (- b) (expt x (- a)))))))

;; TODO: dirichlet

(define-distribution-function poisson (mu)
  (let ((prod 1.0)
        (k 0))
    (loop while (< mu 10)
          for m = (floor (* mu (/ 7.0 8.0)))
          for x = (gamma-int m)
          do (if (<= mu x)
                 (return (+ (binomial (/ mu x) (1- m))))
                 (setf k (+ k m)
                       mu (- mu x))))
    (let ((emu (exp (- mu))))
      (loop do (setf prod (* prod (r)))
               (incf k)
            while (< emu prod))
      (float (1- k) 0f0))))

(define-probability-density-function poisson (x mu)
  (exp (- (* (log mu) x) (lnfact x) mu)))

(define-distribution-function bernoulli (p)
  (if (< (r) p) 1.0 0.0))

(define-probability-density-function bernoulli (x p)
    (cond ((= x 0) (- 1.0 p))
          ((= x 1) p)
          (T 0.0)))

(define-distribution-function binomial (p n)
  (let ((k 0))
    (loop while (< 10 n)
          for a = (1+ (/ n 2.0))
          for b = (1+ (- n a))
          for x = (beta a b)
          do (if (<= p x)
                 (setf n (1- a)
                       p (/ p x))
                 (setf k (+ k a)
                       n (1- b)
                       p (/ (- p x) (- 1 x)))))
    (dotimes (i n k)
      (if (< (r) p) (incf k)))))

(define-probability-density-function binomial (x p n)
  (if (< n x)
      0.0
      (cond ((= p 0)
             (if (= x 0) 1.0 0.0))
            ((= p 1)
             (if (= x n) 1.0 0.0))
            (T
             (exp (+ (lnchoose n x) (* x (log p) (* (- n x) (log (- 1 p))))))))))

(define-distribution-function -binomial (p n)
  (poisson (* (gamma n) (/ (- 1 p) p))))

(define-probability-density-function -binomial (x p n)
  (exp (+ (- (lngamma (+ x n))
             (lngamma n)
             (lngamma (+ x 1)))
          (* n (log p))
          (* x (log (- 1 p))))))

;; TODO: pascal

(define-distribution-function geometric (p)
  (if (= p 1)
      1.0
      (ffloor (1+ (/ (log (r)) (log (- 1 p)))))))

(define-probability-density-function geometric (x p)
  (cond ((= x 0) 0.0)
        ((= x 1) p)
        (T (* p (expt (- 1 p) (1- x))))))

;; TODO: hypergeometric

(define-distribution-function logarithmic (p)
  (let ((v (r)))
    (if (<= p v)
        1.0
        (let* ((c (log (- 1 p)))
               (u (r))
               (q (- 1 (exp (* c u)))))
          (cond ((<= v (* q q)) (ffloor (1+ (/ (log v) (log q)))))
                ((<= v q) 2.0)
                (T 1.0))))))

(define-probability-density-function logarithmic (x p)
  (if (= x 0)
      0.0
      (/ (expt p x) x (log (/ (- 1 p))))))

;; TODO: wishart
