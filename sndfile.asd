(asdf:defsystem "sndfile"
  :description "libsndfile wrapper"
  :version "0.1"
  :author "Jens Thiede"
  :licence "New BSD-Style License"
  :depends-on ("cffi")
  :components ((:file "package")
	       (:file "sndfile-cffi" :depends-on ("package"))
	       (:file "sndfile" :depends-on ("package" "sndfile-cffi"))))
