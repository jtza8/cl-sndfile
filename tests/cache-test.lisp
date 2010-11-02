(in-package #:sndfile)

(defclass cache-test (test-case)
  ())

(def-test-method test-cache ((test cache-test))
  (with-cache (cache :total-items 10 :item-type :int)
    (dotimes (i 10)
      (setf (pointer-ref cache) i)
      (unless (>= i 9) (offset-pointer cache 1)))
    (loop for i from 9 downto 0 do
         (progn (assert-equal i (pointer-ref cache))
                (unless (<= i 0) (offset-pointer cache -1))))
    (move-pointer cache 3)
    (assert-equal 3 (pointer-ref cache))))