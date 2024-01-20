(defpackage #:org.shirakumo.random-sampling
  (:use #:cl #:org.shirakumo.fraf.math)
  (:shadowing-import-from #:org.shirakumo.random-state #:random)
  (:local-nicknames
   (#:random-state #:org.shirakumo.random-state))
  (:export))
