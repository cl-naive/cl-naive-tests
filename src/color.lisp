(in-package :cl-naive-tests)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sgr (code)
    (format nil "~C[~Am" (code-char 27) code)))

(defvar *bold*         (sgr 1))
(defvar *underline*    (sgr 4))
(defvar *blink*        (sgr 5))
(defvar *invert*       (sgr 7))
(defvar *no-bold*      (sgr 22))
(defvar *no-underline* (sgr 24))
(defvar *no-blink*     (sgr 25))
(defvar *no-invert*    (sgr 27))
(defvar *black*        (sgr 30))
(defvar *red*          (sgr 31))
(defvar *green*        (sgr 32))
(defvar *yellow*       (sgr 33))
(defvar *blue*         (sgr 34))
(defvar *magenta*      (sgr 35))
(defvar *cyan*         (sgr 36))
(defvar *white*        (sgr 37))
(defvar *black-back*   (sgr 40))
(defvar *red-back*     (sgr 41))
(defvar *green-back*   (sgr 42))
(defvar *yellow-back*  (sgr 43))
(defvar *blue-back*    (sgr 44))
(defvar *magenta-back* (sgr 45))
(defvar *cyan-back*    (sgr 46))
(defvar *white-back*   (sgr 47))
(defvar *normal*       (sgr 0))


(defvar *use-color* nil)

(defun success-color ()
  (if *use-color*
      (concatenate 'string *black* *green-back*)
      ""))

(defun failure-color ()
  (if *use-color*
      (concatenate 'string *black* *red-back*)
      ""))

(defun error-color ()
  (if *use-color*
      (concatenate 'string *black* *magenta-back*)
      ""))


(defun normal-color ()
  (if *use-color*
      *normal*
      ""))
