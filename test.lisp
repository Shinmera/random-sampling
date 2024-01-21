(defpackage #:org.shirakumo.random-sampling.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:random #:org.shirakumo.random-state)
   (#:sampling #:org.shirakumo.random-sampling)))
(in-package #:org.shirakumo.random-sampling.test)

(define-test random-sampling)
