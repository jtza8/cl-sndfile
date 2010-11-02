(in-package #:sndfile)

(define-condition sound-file-error (error)
  ((message :initarg :message
            :initform ""
            :reader message)))

(defclass sound-file ()
  ((frames :initform 0
           :initarg :frames
           :reader frames)
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
   (read-buffer :initform nil
                :reader read-buffer)
   (read-buffer-count :initarg :read-buffer-size
                      :initform 1024
                      :reader read-buffer-count)
   (write-buffer :initform nil
                 :reader write-buffer)
   (write-buffer-count :initform 1024
                       :reader write-buffer-count)))

(defmacro with-open-sound-file ((variable-name file-name mode &rest key-args)
                                &body body)
  `(let ((,variable-name (open ,file-name ,mode ,@key-args)))
     (unwind-protect (progn ,@body)
     (close ,variable-name))))

(defun open (file-name mode &key
             (frames 0) (sample-rate 0) (channels 0)
             (file-format '(:undefined . :undefined))
             (seekable nil) (sections 1))
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
          (when (or (null-pointer-p file)
                    (not (= (sf_error file) SF_ERR_NO_ERROR)))
            (error 'sound-file-error :message (sf_strerror file)))
          (when (or (eql mode :read) (eql mode :read-write))
            (setf frames (i 'frames)
                  sample-rate (i 'samplerate)
                  channels (i 'channels)
                  file-format (format-to-labels (i 'format))
                  seekable (i 'seekable)))
          (make-instance 'sound-file
                         :file file :frames frames
                         :sample-rate sample-rate
                         :channels channels :file-format file-format
                         :sections sections :seekable seekable))))))

(defmethod close ((sound-file sound-file))
  (sf_close (slot-value sound-file 'file)))

;; (defmethod read-frames ((sound-file sound-file) &optional (count 1024))
;;   (with-slots (read-buffer read-buffer-count) sound-file
;;     (setf read-buffer-count count)
;;     (unless (or (null read-buffer) (null-pointer-p read-buffer))
;;       (foreign-free read-buffer))
;;     (setf read-buffer (foreign-alloc :int))