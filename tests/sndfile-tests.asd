(asdf:defsystem "sndfile-tests"
  :depends-on (#:sndfile #:xlunit)
  :components ((:file "test-package")
	       (:file "test-utils" :depends-on ("test-package"))
	       (:file "test-sound-file" :depends-on ("test-package"))))