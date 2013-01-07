(in-package #:saluto)


(defclass oauth-2.0-module (base-auth-module)
  ((provider-name         :documentation "Name of the OAuth provider")
   (api-host              :documentation "e.g. https://graph.facebook.com, the URI for REST API")
   (oauth-host            :documentation "URI to start OAuth iteraction")
   (app-id                :documentation "OAuth App ID. Provider can call it differently")
   (app-secret-key        :documentation "OAuth secret key")
   (oauth-path            :documentation "The path on a provider's domain where we direct the user for authentication, e.g. /oauth/authorize")
   (access-token-path     :documentation "URI to optain the access_token value on a provider's domain")
   (access-token-method   :documentation "HTTP request method for access_token"
                          :initform :get)
   (post-access-param-via :documentation "If access-token-method is :post then choose method for post: query or data"
                          :initform :query)
   (receiver-path         :documentation "URI on our domain where to redirect on success or failure")
   (error-callback        :documentation "function called on error during authentication process"
                          :initform nil)))


(defgeneric execute (oauth-2.0-module package app-id secret-key))

(defgeneric go-to-provider (oauth-2.0-module))

(defgeneric receiver (oauth-2.0-module session code error?))

(defmacro new-oauth-provider (name)
  (let* ((provider (string-upcase name))
         (provider-kw (intern provider 'keyword))
         (provider-module
           (intern (concatenate 'string provider "-MODULE") '#:saluto))
         (provider-var
           (intern (concatenate 'string "*" provider "-MODULE*") '#:saluto)))
    (push provider-kw *provider-list*)
    (eval `(defclass ,provider-module (oauth-2.0-module)()))
    (eval `(defvar ,provider-var nil))
    (eval `(export ,provider-var))))

#|
(defun make-provider (provider)
  (let (found (find provider *provider-list*))
    (when found
      (let ((module (concatenate 'string (string found)))))
      )))
|#
