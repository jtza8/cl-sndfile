(in-package :sndfile)

(defparameter *test-sounds-path* 
  #p"/Users/jens/Documents/programming/lisp/cl-sndfile/tests/sound-files/")

(defclass sndfile-test (test-case)
  ())

(def-test-method test-open ((test sndfile-test))
  (let* ((file-name (merge-pathnames "test.wav" *test-sounds-path*))
	 (file (open file-name :read)))
    (assert-false (cffi:null-pointer-p file))
    (sf_close file)))