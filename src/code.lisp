(in-package #:saluto)

(defun make-provider (provider app-id app-secret domain)
  "Function creates instance of given provider"
  (let ((found (find provider *provider-list*)))
    (break "~A: ~A" found *provider-list*)
    (when found
      (let* ((provider-str (string found))
             (module (concatenate 'string provider-str +module-str+))
             (variable (intern (concatenate 'string "*" module "*") '#:saluto))
             (value (symbol-value variable))
             (instance nil))

        (when (not value)

          (setf instance                         (make-instance (intern module '#:saluto)))

          (init-module instance)

          (setf
           (slot-value instance 'app-id)          app-id
           (slot-value instance 'app-secret-key)  app-secret
           (slot-value instance 'domain)          domain
           (symbol-value variable)                instance
           value                                  (symbol-value variable)))

        value))))

(defun do-attach-saluto (provider-list)
  (assert (listp provider-list))

  (dolist (i provider-list)
    (multiple-value-bind (module visibility)
        (intern (concatenate 'string (string i) "-MODULE") '#:saluto)
      (princ module))))

(defun attach-saluto (provider-list)
  (do-attach-saluto
      (if (listp provider-list)
          provider-list
          (list provider-list))))
