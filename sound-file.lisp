(in-package #:sndfile)

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
	 :initarg :file)))

(defmacro with-open-sound-file ((variable-name file-name mode &rest key-args)
			       &body body)
  `(let (,variable-name)
     (unwind-protect (setf ,variable-name (open ,file-name ,mode ,@key-args))
       ,@body)
     (close ,variable-name)))

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