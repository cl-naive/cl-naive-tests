(in-package :cl-naive-tests)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sgr (code)
    (format nil "~C[~Am" (code-char 27) code)))

(defconstant +bold+         (sgr 1))
(defconstant +underline+    (sgr 4))
(defconstant +blink+        (sgr 5))
(defconstant +invert+       (sgr 7))
(defconstant +no-bold+      (sgr 22))
(defconstant +no-underline+ (sgr 24))
(defconstant +no-blink+     (sgr 25))
(defconstant +no-invert+    (sgr 27))
(defconstant +black+        (sgr 30))
(defconstant +red+          (sgr 31))
(defconstant +green+        (sgr 32))
(defconstant +yellow+       (sgr 33))
(defconstant +blue+         (sgr 34))
(defconstant +magenta+      (sgr 35))
(defconstant +cyan+         (sgr 36))
(defconstant +white+        (sgr 37))
(defconstant +black-back+   (sgr 40))
(defconstant +red-back+     (sgr 41))
(defconstant +green-back+   (sgr 42))
(defconstant +yellow-back+  (sgr 43))
(defconstant +blue-back+    (sgr 44))
(defconstant +magenta-back+ (sgr 45))
(defconstant +cyan-back+    (sgr 46))
(defconstant +white-back+   (sgr 47))
(defconstant +normal+       (sgr 0))


(defvar *use-color* nil)

(defun success-color ()
  (if *use-color*
      (concatenate 'string +black+ +green-back+)
      ""))

(defun failure-color ()
  (if *use-color*
      (concatenate 'string +black+ +red-back+)
      ""))

(defun error-color ()
  (if *use-color*
      (concatenate 'string +black+ +magenta-back+)
      ""))


(defun normal-color ()
  (if *use-color*
      +normal+
      ""))
