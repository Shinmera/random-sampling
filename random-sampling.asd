(asdf:defsystem random-sampling
  :version "0.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Functions to generate random samples with various distributions"
  :homepage "https://shinmera.com/docs/random-sampling/"
  :bug-tracker "https://shinmera.com/project/random-sampling/issues"
  :source-control (:git "https://shinmera.com/project/random-sampling.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "volumes")
               (:file "distributions")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :random-state
               :3d-math)
  :in-order-to ((asdf:test-op (asdf:test-op :random-sampling/test))))

(asdf:defsystem random-sampling/test
  :version "0.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Tests for the random-sampling system"
  :homepage "https://shinmera.com/docs/random-sampling/"
  :bug-tracker "https://shinmera.com/project/random-sampling/issues"
  :source-control (:git "https://shinmera.com/project/random-sampling.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:random-sampling :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.random-sampling.test)))
