(asdf:defsystem "sndfile-tests"
  :depends-on (#:sndfile #:xlunit)
  :components ((:file "test-package")
               (:file "utils-test" :depends-on ("test-package"))
               (:file "cache-test" :depends-on ("test-package"))
               (:file "sound-file-test" :depends-on ("test-package"))))