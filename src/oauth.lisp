(in-package #:saluto)

(defclass oauth-2.0-module (base-auth-module)
  ((domain                :documentation "My domain to which authentication is attached.")
   (provider-name         :documentation "Name of the OAuth provider")
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
   (query-params          :documentation "Append parameters to redirect query if needed.")
   (receiver-path         :documentation "URI on our domain where to redirect on success or failure")
   (error-callback        :documentation "function called on error during authentication process"
                          :initform nil)))

(defgeneric execute (oauth-2.0-module package app-id secret-key))
(defgeneric init-module (oauth-2.0-module))
(defgeneric go-to-provider (oauth-2.0-module))
(defgeneric receiver (oauth-2.0-module session code error?))
(defgeneric build-goto-path (oauth-2.0-module session-str))

(defmacro new-oauth-provider (name &key init-values goto-fun receiver-fun)
  (let* ((provider         (string-upcase name))
         (provider-low     (string-downcase name))
         (provider-kw      (intern provider 'keyword))
         (provider-module  (intern (concatenate 'string provider +module-str+)         '#:saluto))
         (provider-var     (intern (concatenate 'string "*" provider +module-str+ "*") '#:saluto))
         (s-route-go       (intern (concatenate 'string provider ".GO-TO-PROVIDER")    '#:saluto))
         (s-route-receiver (intern (concatenate 'string provider ".RECEIVER")          '#:saluto))
         (route-go         (concatenate 'string "/auth/go/" provider-low "/"))
         (route-receiver   (concatenate 'string "/auth/receiver/" provider-low "/:session/")))
    (push provider-kw *provider-list*)


    `(progn
       (defclass ,provider-module (oauth-2.0-module)())
       (defvar ,provider-var nil)
       (export ',provider-var)
       (export ',provider-module)
       (export ',s-route-go)
       (export ',s-route-receiver)

       (defmethod attach-routes (,provider-module)

`         (restas:define-route ,s-route-go (,route-go
                                           :method :get
                                           :content-type "text/html")
           (go-to-provider ,provider-var))

         (setf (documentation ',s-route-go 'function)
               (format nil
                       "This RESTAS route function begins authorization process for provider '~A'."
                       ,name))

         (restas:define-route ,s-route-receiver (,route-receiver
                                                 :method :get
                                                 :content-type "text/html")
           (receiver ,provider-var
                     session
                     (hunchentoot:parameter "code")
                     (hunchentoot:parameter "error")))

         (setf (documentation ',s-route-receiver 'function)
               (format nil
                       "This RESTAS route function receive authorization answer from provider '~A'."
                       ,name))
         t)

       (defmethod init-module (,provider-module)
         (dolist (i ,init-values)
           (setf (slot-value ,provider-module (car i))
                 (cdr i)))
                                        ; receiver path constructed at build-goto-path
         (setf (slot-value ,provider-module 'receiver-path)
               (concatenate 'string (concatenate 'string "/auth/receiver/" ,provider-low "/~A/"))))

       (defmethod go-to-provider (,provider-module)
         (let ((fn ,goto-fun ))
           (funcall fn ,provider-module)))

       (defmethod receiver (,provider-module session code error?)
         (let ((fn ,receiver-fun))
           (funcall fn ,provider-module session code error?))))))

(defmethod build-goto-path ((module oauth-2.0-module) session-str)
  (assert (slot-value module 'domain))
  (assert (slot-value module 'app-id))
  (assert (slot-value module 'app-secret-key))
  (assert (slot-value module 'receiver-path))
  (let ((res ""))
    (setf res
          (concatenate 'string
                       (slot-value module 'oauth-host)
                       (slot-value module 'oauth-path)
                       "?"
                       "client_id="
                       (slot-value module 'app-id)))

    (dolist (i (slot-value module 'query-params))
      (setf res (concatenate 'string res
                             "&" (car i) "=" (cdr i))))

    (setf res (concatenate 'string res
                           "&" "redirect_uri="

                           (drakma:url-encode
                            (concatenate 'string
                                         (slot-value module 'domain)
                                         (format nil
                                                 (slot-value module 'receiver-path)
                                                 session-str)) :latin-1)))))
