(asdf:defsystem "sndfile-tests"
  :depends-on (#:sndfile #:xlunit)
  :components ((:file "test-package")
	       (:file "test-sndfile" :depends-on ("test-package"))))
