(in-package #:saluto)

(defun make-provider (provider app-id app-private app-secret domain store-userinfo-fun)
  "Function creates instance of given provider"
  
  (let ((found (find provider *providers*)))
    (when found
      (let* ((provider-str (string found))
             (module (concatenate 'string provider-str +module-str+))
             (variable (intern (concatenate 'string "*" module "*") :saluto))
             (value (symbol-value variable))
             (instance nil))

        (when (not value)

          (setf instance                         (make-instance (intern module :saluto)))
          (init-module instance)

          (setf
           (slot-value instance 'app-id)             app-id
           (slot-value instance 'app-private-key)    app-private
           (slot-value instance 'app-secret-key)     app-secret
           (slot-value instance 'domain)             domain
           (slot-value instance 'store-userinfo-fun) (if store-userinfo-fun
                                                         store-userinfo-fun
                                                         #'(lambda (user-info)
                                                             (declare (ignore user-info))
                                                             (error "Store user info function not implemented")))

           (symbol-value variable)                   instance
           value                                     (symbol-value variable)))

        value))))

(defun attach-saluto (provider-list store-fun)
  (assert (listp provider-list))
  (dolist (i provider-list)
    (let ((module      (getf i :module))
          (app-id      (getf i :app-id))
          (app-private (getf i :app-private))
          (app-secret  (getf i :app-secret))
          (domain      (getf i :domain)))
      (let ((provider (make-provider module app-id app-private app-secret domain store-fun)))
        (attach-routes provider)))))


