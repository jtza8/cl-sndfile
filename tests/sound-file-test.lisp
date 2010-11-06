(in-package #:sndfile)

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

(def-test-method test-update-read-cache ((test sound-file-test))
  (with-open-sound-file (file (merge-pathnames "test.wav" *test-sounds-path*)
                         :read :read-cache-size 3)
    (dotimes (i 3) (read-frame file))
    (assert-not-eql (read-frame file) (read-frame file))))

(def-test-method test-read-frame ((test sound-file-test))
  (with-open-sound-file (file (merge-pathnames "test.wav" *test-sounds-path*)
                         :read)
    (with-slots (read-cache) file
      (assert-true (null read-cache) "non-nil read-cache")
      (assert-equal (read-frame file) -0.096954345703125d0)
      (assert-equal (read-frame file) 0.069427490234375d0)))
  (with-open-sound-file (file (merge-pathnames "stereo.wav" *test-sounds-path*)
                         :read)
    (multiple-value-bind (left right) (read-frame file)
      (assert-equal 0.0d0 left)
      (assert-equal -6.103515625d-5 right))))

(def-test-method test-write-frame ((test sound-file-test))
  (with-open-sound-file (file (merge-pathnames "output.wav" *test-sounds-path*)
                         :write :file-format '(:wav . :double) :channels 1
                         :sample-rate 44100 :frames 10 :sections 1)
    (loop for i from 0.1d0 upto 1.0d0 by 0.1d0 do (write-frame file i)))
  (with-open-sound-file (file (merge-pathnames "output.wav" *test-sounds-path*)
                         :read)
    (loop for i from 0.1d0 upto 1.0d0 by 0.1d0
          do (assert-equal i (read-frame file)))))

(def-test-method test-seek ((test sound-file-test))
  (with-open-sound-file (file (merge-pathnames "output.wav" *test-sounds-path*)
                         :write :file-format '(:wav . :double) :channels 2
                         :sample-rate 44100 :frames 10 :sections 1)
    (loop for i from 0.1d0 upto 1.0d0 by 0.1d0
          do (write-frame file i (- 1.0d0 i))))
  (with-open-sound-file (file (merge-pathnames "output.wav" *test-sounds-path*)
                         :read)
    (loop for i from 0.1d0 upto 0.4d0 by 0.1d0
          do (multiple-value-bind (left right) (read-frame file)
               (assert-equal i left)
               (assert-equal (- 1 i) right)))
    (seek-frame file 1)
    (multiple-value-bind (left right) (read-frame file)
      (assert-equal 0.2d0 left)
      (assert-equal 0.8d0 right))
    (seek-frame file -1 :end)
    (multiple-value-bind (left right) (read-frame file)
      (assert-equal 0.8d0 left)
      (assert-equal 0.2d0 right))))


(def-test-method test-frame-index ((test sound-file-test))
  (with-open-sound-file (file (merge-pathnames "test.wav" *test-sounds-path*)
                         :read)
    (assert-equal 0 (frame-index file))
    (read-frame file)
    (assert-equal 1024 (frame-index file))
    (read-frame file)
    (assert-equal 1024 (frame-index file))))