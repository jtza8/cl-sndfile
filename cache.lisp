(in-package #:sndfile)

(defclass cache ()
  ((start-address :reader start-address)
   (end-address :reader end-address)
   (pointer :reader pointer)
   (total-items :initarg :total-items
                :initform (error "total-items not specified")
                :reader total-items)
   (item-type :initarg :item-type
              :initform (error "item-type not specified")
              :reader item-type)
   item-size))


(defmethod initialize-instance :after ((cache cache) &key)
  (with-slots (start-address end-address pointer
               total-items item-type item-size) cache
    (setf start-address (foreign-alloc item-type :count total-items)
          pointer start-address
          item-size (foreign-type-size item-type)
          end-address (inc-pointer start-address
                                   (* item-size (1- total-items))))))

(defmethod free ((cache cache))
  (foreign-free (slot-value cache 'start-address)))

(defmethod pointer-ref ((cache cache))
  (with-slots (pointer item-type) cache
    (mem-ref pointer item-type)))

(defmethod (setf pointer-ref) (new-value (cache cache))
  (with-slots (pointer item-type) cache
    (setf (mem-ref pointer item-type) new-value)))

(defmethod within-bounds-p ((cache cache) address)
  (with-slots (start-address end-address) cache
    (<= (pointer-address start-address) address (pointer-address end-address))))

(defmethod assert-within-bounds ((cache cache) address &optional
                                 (message "offset out of bounds"))
  (assert (within-bounds-p cache address) () message))

(defmethod offset-pointer ((cache cache) item-delta)
  (with-slots (pointer item-size) cache
    (assert-within-bounds cache (+ (pointer-address pointer)
                                   (* item-delta item-size)))
    (incf-pointer pointer (* item-delta item-size))))

(defmethod move-pointer ((cache cache) item-delta)
  (with-slots (start-address pointer item-size) cache
    (let ((address (+ (pointer-address start-address)
                      (* item-delta item-size))))
    (assert-within-bounds cache address)
    (setf pointer (make-pointer address)))))

(defmacro with-cache ((variable &rest initargs) &body body)
  `(let ((,variable (make-instance 'cache ,@initargs)))
    (unwind-protect (progn ,@body) (free cache))))