(in-package #:sndfile)

(defstruct info
  (frames nil)
  (sample-rate 0 :type integer)
  (channels 0 :type integer)
  (format :undefined)
  (sections 0 :type integer)
  (seekable nil :type boolean))

(defun open (file-name mode &optional (info nil))
  (let ((mode (ecase mode
		(:read SFM_READ)
		(:write SFM_WRITE)
		(:read-write SFM_RDWR)))
	(file-name (namestring file-name)))
    (cffi:with-foreign-object (info 'sf_info)
      (sf_open file-name mode info))))