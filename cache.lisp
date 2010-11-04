(in-package #:sndfile)

(define-condition cache-index-error (error)
  ((message :initform ""
            :initarg :message
            :reader message)))

(defclass cache ()
  ((start-address :reader start-address)
   (end-address :reader end-address)
   (iterator :reader iterator)
   (total-items :initarg :total-items
                :initform (error "total-items not specified")
                :reader total-items)
   (item-type :initarg :item-type
              :initform (error "item-type not specified")
              :reader item-type)
   item-size))


(defmethod initialize-instance :after ((cache cache) &key)
  (with-slots (start-address end-address iterator
               total-items item-type item-size) cache
    (setf start-address (foreign-alloc item-type :count total-items)
          iterator start-address
          item-size (foreign-type-size item-type)
          end-address (inc-pointer start-address
                                   (* item-size (1- total-items))))))

(defmethod free ((cache cache))
  (with-slots (start-address end-address iterator) cache
    (foreign-free start-address)
    (setf start-address (null-pointer)
          end-address (null-pointer)
          iterator (null-pointer))))

(defmethod within-bounds-p ((cache cache) address)
  (with-slots (start-address end-address) cache
    (<= (pointer-address start-address) address (pointer-address end-address))))

(defmethod assert-within-bounds ((cache cache) address &optional
                                 (message "offset out of bounds"))
  (assert (within-bounds-p cache address) () 
          'cache-index-error :message message))

(defmethod out-of-cache-p ((cache cache))
  (null-pointer-p (slot-value cache 'iterator)))

(defmethod assert-not-out-of-cache ((cache cache) &optional
                                    (message "past end of the cache"))
  (assert (not (out-of-cache-p cache)) ()
          'cache-index-error :message message))

(defmethod iterator-value ((cache cache))
  (assert-not-out-of-cache cache)
  (with-slots (iterator item-type) cache
    (mem-ref iterator item-type)))

(defmethod (setf iterator-value) (new-value (cache cache))
  (with-slots (iterator item-type) cache
    (setf (mem-ref iterator item-type) new-value)))

(defmethod offset-iterator ((cache cache) item-delta)
  (with-slots (iterator start-address item-size) cache
    (when (out-of-cache-p cache)
      (setf iterator start-address))
    (if (within-bounds-p cache (+ (pointer-address iterator)
                                  (* item-delta item-size)))
        (incf-pointer iterator (* item-delta item-size))
        (setf iterator (null-pointer)))))

(defmethod move-iterator ((cache cache) item-delta)
  (with-slots (start-address iterator item-size) cache
    (let ((address (+ (pointer-address start-address)
                      (* item-delta item-size))))
    (assert-within-bounds cache address)
    (setf iterator (make-pointer address)))))

(defmethod iterator-at-end-p ((cache cache))
  (with-slots (iterator end-address) cache
    (pointer-eq iterator end-address)))

(defmethod iterator-at-start-p ((cache cache))
  (with-slots (iterator start-address) cache
    (pointer-eq iterator start-address)))

(defmethod read-entry ((cache cache))
  (assert-not-out-of-cache cache)
  (let ((result (iterator-value cache)))
    (offset-iterator cache 1)
    result))

(defmethod write-entry ((cache cache) entry)
  (assert-not-out-of-cache cache)
  (let ((result (setf (iterator-value cache) entry)))
    (offset-iterator cache 1)
    result))

(defmacro with-cache ((variable &rest initargs) &body body)
  `(let ((,variable (make-instance 'cache ,@initargs)))
    (unwind-protect (progn ,@body) (free cache))))