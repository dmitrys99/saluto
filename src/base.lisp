(in-package #:saluto)


(defclass base-auth-module () ())

(defun do-attach-saluto (provider-list)
  (assert (listp provider-list))

  (dolist (i provider-list)
    (multiple-value-bind (module visibility)
        (intern (concatenate 'string (string i) "-MODULE") '#:saluto)
      
      (princ module))
    ))

(defun attach-saluto (provider-list)
  (do-attach-saluto
      (if (listp provider-list)
          provider-list
          (list provider-list))))

(defparameter *provider-list* ())

(defparameter +module-str+ "-MODULE")
