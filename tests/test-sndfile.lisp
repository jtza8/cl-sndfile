(in-package :sndfile)

(defparameter *test-sounds-path* 
  #p"/Users/jens/Documents/programming/lisp/cl-sndfile/tests/sound-files/")

(defclass sndfile-test (test-case)
  ())

(def-test-method test-format-to-labels ((test sndfile-test))
  (assert-equal '(:ogg . :pcm-32) 
		(format-to-labels (logior SF_FORMAT_OGG SF_FORMAT_PCM_32))))

(def-test-method test-labels-to-format ((test sndfile-test))
  (assert-equal (logior SF_FORMAT_OGG SF_FORMAT_PCM_32)
		(labels-to-format '(:ogg . :pcm-32))))

(def-test-method test-c-info-to-lisp-info ((test sndfile-test))
  (with-foreign-object (c-info 'sf_info)
    (with-foreign-slots ((frames samplerate channels 
			  format sections seekable)
			 c-info sf_info)
      (setf frames (null-pointer)
      	    samplerate 44000
      	    channels 2
      	    format (logior SF_FORMAT_AIFF SF_FORMAT_PCM_32)
      	    sections 2
      	    seekable SF_TRUE))
    (let ((info (c-info-to-lisp-info c-info)))
      (assert-true (null-pointer-p (info-frames info)))
      (assert-equal 44000 (info-sample-rate info))
      (assert-equal 2 (info-channels info))
      (assert-equal '(:aiff . :pcm-32) (info-format info))
      (assert-equal 2 (info-sections info))
      (assert-true (info-seekable info)))))

(def-test-method test-open ((test sndfile-test))
  (let* ((file-name (merge-pathnames "test.wav" *test-sounds-path*)))
    (multiple-value-bind (file info) (open file-name :read)
      (sf_close file)
      (assert-false (cffi:null-pointer-p file))
      (assert-true (info-p info)))))