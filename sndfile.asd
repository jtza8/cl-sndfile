(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem "sndfile"
  :description "libsndfile wrapper"
  :version "0.1"
  :author "Jens Thiede"
  :licence "New BSD-Style License"
  :depends-on ("cffi")
  :serial t
  :components ((:file "package")
               (cffi-grovel:grovel-file "grovel")
               (:file "sndfile-cffi" :depends-on ("package"))
               (:file "utils" :depends-on ("package" "sndfile-cffi"))
               (:file "cache" :depends-on ("package"))
               (:file "sound-file"
                :depends-on ("package" "utils" "cache" "sndfile-cffi"))))
