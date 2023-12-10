(in-package :cl-naive-tests)

(defgeneric hash-table-projection (keys table)
  (:documentation "Returns a new hash-table containing the same values as in TABLE but only for the given keys.
KEYS can be a list of keys, or a generalized boolean function called with key and value."))

(defmethod hash-table-projection ((keys null) (table hash-table))
  (make-hash-table :test (hash-table-test table)
                   :size 0
                   :rehash-size (hash-table-rehash-size table)
                   :rehash-threshold (hash-table-rehash-threshold table)))

(defmethod hash-table-projection ((keys cons) table)
  (let ((result
          (make-hash-table :test (hash-table-test table)
                           :size (length keys)
                           :rehash-size (hash-table-rehash-size table)
                           :rehash-threshold (hash-table-rehash-threshold table))))
    (dolist (key keys result)
      (setf (gethash key result) (gethash key table)))))

(defmethod hash-table-projection ((keys function) table)
  (let ((result
          (make-hash-table :test (hash-table-test table)
                           :size (hash-table-count table)
                           :rehash-size (hash-table-rehash-size table)
                           :rehash-threshold (hash-table-rehash-threshold table))))
    (maphash (lambda (key value)
               (when (funcall keys key value)
                 (setf (gethash key result) value)))
             table)
    result))


(defun ensure-list (item)
  (if (listp item)
      item
      (list item)))
