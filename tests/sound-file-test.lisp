(in-package :sndfile)

(defclass sound-file-test (test-case)
  ())

(def-test-method test-open-headers ((test sound-file-test))
  (with-open-sound-file (file (merge-pathnames "test.wav" *test-sounds-path*)
                         :read)
    (assert-equal 44100 (frames file))
    (assert-equal 44100 (sample-rate file))
    (assert-equal 1 (channels file))
    (assert-equal '(:wav . :pcm-16) (file-format file))
    (assert-equal 1 (sections file))
    (assert-equal t (seekable file)))
  (assert-condition 'sound-file-error (open (merge-pathnames "non-existant.wav"
                                                             *test-sounds-path*)
                                            :read))
  (assert-condition 'sound-file-error (open (merge-pathnames "corrupt.wav"
                                                             *test-sounds-path*)
                                            :read)))

(def-test-method test-read-frame ((test sound-file-test))
  (with-open-sound-file (file (merge-pathnames "test.wav" *test-sounds-path*)
                         :read)
    (with-slots (read-buffer) file
      (assert-true (null read-buffer) "non-nil read-buffer")
      (assert-equal 1024 (read-frames file 1024))
      (assert-false (null-pointer-p read-buffer) "unexpected null-pointer"))))