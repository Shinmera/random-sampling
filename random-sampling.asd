(asdf:defsystem random-sampling
  :version "0.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Functions to generate random samples with various distributions"
  :homepage "https://Shinmera.github.io/random-sampling/"
  :bug-tracker "https://github.com/Shinmera/random-sampling/issues"
  :source-control (:git "https://github.com/Shinmera/random-sampling.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "volumes")
               (:file "distributions")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :random-state
               :3d-math)
  :in-order-to ((asdf:test-op (asdf:test-op :random-sampling-test))))
