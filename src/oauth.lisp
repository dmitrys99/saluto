(in-package #:saluto)

(defclass oauth-2.0-module (base-auth-module)
  ((provider-name         :documentation "Name of the OAuth provider")
   (api-host              :documentation "e.g. https://graph.facebook.com, the URI for REST API")
   (oauth-host            :documentation "URI to start OAuth iteraction")
   (app-id                :documentation "OAuth App ID. Provider can call it differently")
   (app-secret-key        :documentation "OAuth secret key")
   (oauth-path            :documentation "The path on a provider's domain where we direct the user for authentication, e.g. /oauth/authorize"
                          :initform "/oauth/authorize")
   (access-token-path     :documentation "URI to optain the access_token value on a provider's domain")
   (access-token-method   :documentation "HTTP request method for access_token"
                          :initform :get)
   (post-access-param-via :documentation "If access-token-method is :post then choose method for post: query or data"
                          :initform :query)
   (receiver-path         :documentation "URI on our domain where to redirect on success or failure")
   (error-callback        :documentation "function called on error during authentication process"
                          :initform nil)))

(defgeneric execute (oauth-2.0-module package app-id secret-key))

(defgeneric init-module (oauth-2.0-module))

(defgeneric go-to-provider (oauth-2.0-module))

(defgeneric receiver (oauth-2.0-module session code error?))

(defmacro new-oauth-provider (name)
  (let* ((provider (string-upcase name))
         (provider-kw (intern provider 'keyword))
         (provider-module
           (intern (concatenate 'string provider +module-str+) '#:saluto))
         (provider-var
           (intern (concatenate 'string "*" provider +module-str+ "*") '#:saluto)))
    (push provider-kw *provider-list*)

    (eval
     `(progn
        (defclass ,provider-module (oauth-2.0-module)())
        (defvar ,provider-var nil)
        (export ,provider-var)))))


(defun make-provider (provider app-id app-secret)
  "Function creates instance of given provider"
  (let ((found (find provider *provider-list*)))
    (when found
      (let* ((provider-str (string found))
             (module (concatenate 'string provider-str +module-str+))
             (variable (intern (concatenate 'string "*" module "*")))
             (value (symbol-value variable))
             (instance nil))
        (when (not value)

          (setf instance
                (make-instance (intern module)))

          (init-module instance)

          (setf

           (slot-value instance 'app-id)
           app-id

           (slot-value instance 'app-secret-key)
           app-secret
           
           (symbol-value variable)
           instance

           value
           (symbol-value variable)))

        value))))

