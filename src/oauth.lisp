(in-package #:saluto)

(defclass oauth-2.0-module (base-auth-module)
  ((domain                :documentation "My domain to which authentication is attached.")
   (provider-name         :documentation "Name of the OAuth provider")
   (api-host              :documentation "e.g. https://graph.facebook.com, the URI for REST API"
                          :reader        api-host)
   (oauth-host            :documentation "URI to start OAuth iteraction")
   (app-id                :documentation "OAuth App ID. Provider can call it differently"
                          :reader        app-id)
   (app-private-key       :documentation "Application private key"
                          :reader        app-private)
   (app-secret-key        :documentation "OAuth secret key"
                          :reader        app-secret)
   (oauth-path            :documentation "The path on a provider's domain where we direct the user for authentication, e.g. /oauth/authorize"
                          :initform "/oauth/authorize")
   (access-token-path     :documentation "URI to optain the access_token value on a provider's domain")
   (access-token-method   :documentation "HTTP request method for access_token"
                          :initform :get)
   (post-access-param-via :documentation "If access-token-method is :post then choose method for post: query or data"
                          :initform :query)
   (query-params          :documentation "Append parameters to redirect query if needed.")
   (token-params          :documentation "List of parameter to access token query.")
   (receiver-path         :documentation "URI on our domain where to redirect on success or failure")

   (store-userinfo-fun    :documentation "User defined function which stores user info data received from provider."
                          :initform nil)

   (error-callback        :documentation "function called on error during authentication process"
                          :initform nil)))

(defgeneric execute (oauth-2.0-module package app-id secret-key))
(defgeneric init-module (oauth-2.0-module))
(defgeneric go-to-provider (oauth-2.0-module))
(defgeneric receiver (oauth-2.0-module session code error?))
(defgeneric build-goto-path (oauth-2.0-module session-str))
(defgeneric full-receiver-path (oauth-2.0-module session-str))
(defgeneric prepare-access-token-request (oauth-2.0-module code))
(defgeneric prepare-userinfo (oauth-2.0-module params))
(defgeneric parse-userinfo (oauth-2.0-module answer))

(defmacro new-oauth-provider (name
                              &key
                              init-values
                              goto-fun
                              receiver-fun
                              prepare-userinfo-fun
                              parse-userinfo-fun)

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

    ;; (break "~A ~A ~A ~A ~A" provider-var provider-kw provider-module s-route-go s-route-receiver)

    `(progn
       (defclass ,provider-module (oauth-2.0-module)())
       (defvar ,provider-var nil)
       (export ',provider-var)
       (export ',provider-module)
       (export ',s-route-go)
       (export ',s-route-receiver)
       (export 'auth.logout)
       (export 'attach-routes)

       (defmethod attach-routes (,provider-module)

         (restas:define-route ,s-route-go (,route-go
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

         (restas:define-route auth.logout ("/auth/logout/"
                                           :method :get
                                           :content-type "text/html")
           (logout)
           (redirect "/"))

         t)

       (defmethod init-module (,provider-module)
         (dolist (i ,init-values)
           (setf (slot-value ,provider-module (car i))
                 (cdr i)))
         ;; receiver path is constructed at build-goto-path
         (setf (slot-value ,provider-module 'receiver-path)
               (concatenate 'string (concatenate 'string "/auth/receiver/" ,provider-low "/~A/"))))

       (defmethod prepare-userinfo (,provider-module params)
         (let ((fn ,prepare-userinfo-fun))
           (funcall fn ,provider-module params)))

       (defmethod parse-userinfo (,provider-module answer)
         (let ((fn ,parse-userinfo-fun))
           (funcall fn ,provider-module answer)))

       (defmethod go-to-provider (,provider-module)
         (let ((fn ,goto-fun))
           (funcall fn ,provider-module)))

       (defmethod receiver (,provider-module session code error?)
         (let ((fn ,receiver-fun))
           (funcall fn ,provider-module session code error?))))))

(defmethod build-goto-path ((module oauth-2.0-module) session-str)
  (assert (slot-value module 'domain))
  (assert (slot-value module 'app-id))
  (assert (slot-value module 'app-secret-key))
  (assert (slot-value module 'app-private-key))
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
                           (full-receiver-path module session-str)))
    res))

(defmethod prepare-access-token-request ((module oauth-2.0-module) code)
  (let ((parameters (slot-value module 'token-params))
        (token-url (concatenate 'string
                                (slot-value module 'oauth-host)
                                (slot-value module 'access-token-path)))
        (method (slot-value module 'access-token-method))
        (via (slot-value module 'post-access-param-via))
        (res nil))

    (push (cons "client_id"     (slot-value module 'app-id))           parameters)
    (push (cons "client_secret" (slot-value module 'app-private-key))  parameters)
    (push (cons "code" code)                                           parameters)
    (push (cons "redirect_uri"  (full-receiver-path module (session))) parameters)

    (setf parameters (reverse parameters))

    (setf res (list token-url
                    :method method
                    :content-length t))

    (setf res
          (append res
                  (if (eql via :DATA)
                      (list :content (concatenate-params parameters))
                      (list :parameters parameters))))
    res))

(defmethod full-receiver-path (oauth-2.0-module session-str)
  (drakma:url-encode
   (concatenate 'string
                (slot-value oauth-2.0-module 'domain)
                (format nil
                        (slot-value oauth-2.0-module 'receiver-path)
                        session-str))
   :latin-1))
