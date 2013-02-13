(in-package #:saluto)

(defun make-provider (provider app-id app-private app-secret domain)
  "Function creates instance of given provider"
  (let ((found (find provider *provider-list*)))
    ;; (break "~A: ~A" found *provider-list*)
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
           (slot-value instance 'app-private-key) app-private
           (slot-value instance 'app-secret-key)  app-secret
           (slot-value instance 'domain)          domain
           (symbol-value variable)                instance
           value                                  (symbol-value variable)))

        value))))

(defun attach-saluto (provider-list)
  (assert (listp provider-list))
  ;; (break "attach-saluto (package): ~A" (package-name package))
  (dolist (i provider-list)
    (let ((module      (getf i :module))
          (app-id      (getf i :app-id))
          (app-private (getf i :app-private))
          (app-secret  (getf i :app-secret))
          (domain      (getf i :domain)))
      ;; (break)
      (let ((provider  (make-provider module app-id app-private app-secret domain)))
        (attach-routes provider)))))


