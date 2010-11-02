(in-package :sndfile)

(defclass sndfile-test (test-case)
  ())

(def-test-method test-format-to-labels ((test sndfile-test))
  (assert-equal '(:ogg . :pcm-32) 
                (format-to-labels (logior SF_FORMAT_OGG SF_FORMAT_PCM_32))))

(def-test-method test-labels-to-format ((test sndfile-test))
  (assert-equal (logior SF_FORMAT_OGG SF_FORMAT_PCM_32)
                (labels-to-format '(:ogg . :pcm-32))))