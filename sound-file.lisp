(in-package #:sndfile)

(define-condition sound-file-error (error)
  ((message :initarg :message
            :initform ""
            :reader message))
  (:report (lambda (condition stream)
             (format stream "SOUND-FILE-ERROR: ~a" (message condition)))))

(defclass sound-file ()
  ((frames :initform 0
           :initarg :frames
           :reader frames)
   (frame-index :initform 0
                :reader frame-index)
   (sample-rate :initform 0
                :initarg :sample-rate
                :reader sample-rate)
   (channels :initform 0
             :initarg :channels
             :reader channels)
   (file-format :initform '(:undefined . :undefined)
                :initarg :file-format
                :reader file-format)
   (seekable :initform nil
             :initarg :seekable
             :reader seekable)
   (sections :initform 0
             :initarg :sections
             :reader sections)
   (file :initform nil
         :initarg :file)
   (file-mode :initform (error "no file mode specified")
              :initarg :file-mode
              :reader file-mode)
   (read-cache :initform nil
               :reader read-cache)
   (read-cache-size :initarg :read-cache-size
                    :initform 1024
                    :reader read-cache-size)
   (write-cache :initform nil
                :reader write-cache)
   (write-cache-size :initarg :write-cache-size
                     :initform 1024
                     :reader write-cache-size)))

(defun assert-no-error (file)
  (assert (= (sf_error file) SF_ERR_NO_ERROR) ()
                'sound-file-error :message (sf_strerror file)))

(defun open (file-name mode &key
             (frames 0) (sample-rate 0) (channels 0)
             (file-format '(:undefined . :undefined))
             (seekable nil) (sections 1)
             (read-cache-size 1024) (write-cache-size 1024))
  (let ((c-mode (ecase mode
                  (:read SFM_READ)
                  (:write SFM_WRITE)
                  (:read-write SFM_RDWR)))
        (file-name (namestring file-name)))
    (with-foreign-object (info 'sf_info)
      (macrolet ((i (slot-name)
                   `(foreign-slot-value info 'sf_info ,slot-name)))
        (when (eql mode :write)
          (setf (i 'frames) frames
                (i 'samplerate) sample-rate
                (i 'channels) channels
                (i 'format) (labels-to-format file-format)
                (i 'seekable) seekable))
        (let ((file (sf_open file-name c-mode info)))
          (assert-no-error file)
          (when (or (eql mode :read) (eql mode :read-write))
            (setf frames (i 'frames)
                  sample-rate (i 'samplerate)
                  channels (i 'channels)
                  file-format (format-to-labels (i 'format))
                  seekable (i 'seekable)))
          (make-instance 'sound-file
                         :file file :file-mode mode
                         :frames frames :sample-rate sample-rate
                         :channels channels :file-format file-format
                         :sections sections :seekable seekable
                         :read-cache-size read-cache-size
                         :write-cache-size write-cache-size))))))

(defmethod close ((sound-file sound-file))
  (with-slots (file) sound-file
    (clear-read-cache sound-file)
    (clear-write-cache sound-file)
    (sf_close file)))

(defmethod clear-read-cache ((sound-file sound-file))
  (with-slots (read-cache) sound-file
    (unless (null read-cache)
      (free read-cache))
    (setf read-cache nil)))

(defmethod clear-write-cache ((sound-file sound-file))
  (with-slots (write-cache) sound-file
    (unless (null write-cache)
      (flush sound-file)
      (free write-cache))
    (setf write-cache nil)))

(defmethod update-read-cache ((sound-file sound-file))
  (with-slots (file read-cache read-cache-size channels frame-index) sound-file
    (when (or (null read-cache)
              (out-of-cache-p read-cache))
      (unless (null read-cache) (free read-cache))
      (setf read-cache
            (make-instance 'cache 
                           :item-type :double
                           :total-items (* read-cache-size channels)))
      (let ((frame-count (sf_readf_double file (start-address read-cache)
                                          read-cache-size)))
        (assert-no-error file)
        (incf frame-index frame-count)
        frame-count))))

(defmethod read-frame ((sound-file sound-file))
  (update-read-cache sound-file)
  (with-slots (read-cache channels) sound-file
    (apply #'values (loop for i from 1 upto channels
                          collect (read-entry read-cache)))))

(defmethod flush ((sound-file sound-file))
  (with-slots (file frames frame-index write-cache write-cache-size) sound-file
    (let ((frame-write-count (min write-cache-size (- frames frame-index))))
      (unless (null write-cache)
        (sf_write_double file (start-address write-cache) frame-write-count)
        (incf frame-index frame-write-count)
        frame-write-count))))

(defmethod update-write-cache ((sound-file sound-file))
  (with-slots (file frames frame-index write-cache write-cache-size channels) 
      sound-file
    (when (or (null write-cache)
              (out-of-cache-p write-cache))
      (unless (null write-cache) (flush sound-file))
      (setf write-cache
            (make-instance 'cache
                           :item-type :double
                           :total-items (* (min write-cache-size
                                                (- frames frame-index))
                                           channels))))))
        
(defmethod write-frame ((sound-file sound-file) &rest samples)
  (update-write-cache sound-file)
  (with-slots (write-cache channels) sound-file
    (assert (= (length samples) channels) ()
            "number of channels not equal to number of samples")
    (loop for sample in samples do (write-entry write-cache sample))
    samples))

(defmethod seek-frame ((sound-file sound-file) frame-delta
                       &optional (position :start))
  (with-slots (file read-cache write-cache frame-index frames channels)
      sound-file
    (sf_seek file frame-delta (ecase position
                                (:start SEEK_SET)
                                (:current SEEK_CUR)
                                (:end SEEK_END)))
    (assert-no-error file)
    (clear-read-cache sound-file)
    (clear-write-cache sound-file)
    (ecase position
      (:start (setf frame-index frame-delta))
      (:current (incf frame-index frame-delta))
      (:end (setf frame-index (+ frames frame-delta))))))

(defmacro with-open-sound-file ((variable-name file-name mode &rest key-args)
                                &body body)
  `(let ((,variable-name (open ,file-name ,mode ,@key-args)))
     (unwind-protect (progn ,@body)
     (close ,variable-name))))