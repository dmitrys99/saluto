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
   (encode-uri            :documentation "redirect_uri has to be encoded?"
                          :initform nil)

   (store-userinfo-fun    :documentation "User defined function which stores user info data received from provider."
                          :initform nil)

   (error-callback        :documentation "function called on error during authentication process"
                          :initform nil)))

(defgeneric execute (oauth-2.0-module package app-id secret-key))
(defgeneric init-module (oauth-2.0-module))
(defgeneric go-to-provider (oauth-2.0-module))
(defgeneric attach-routes (oauth-2.0-module))
(defgeneric receiver (oauth-2.0-module session code error?))
(defgeneric build-goto-path (oauth-2.0-module session-str))
(defgeneric full-receiver-path (oauth-2.0-module session-str))
(defgeneric prepare-access-token-request (oauth-2.0-module code))
(defgeneric prepare-userinfo-request (oauth-2.0-module params))
(defgeneric parse-userinfo (oauth-2.0-module answer))
(defgeneric store-userinfo (oauth-2.0-module userinfo))

(defmacro new-oauth-provider (name
                              &key
                              init-values
                              goto-fun
                              receiver-fun
                              prepare-userinfo-fun
                              parse-userinfo-fun
                              (anti-csrf :unique-redirect-uri)) ;; Possible values:
                                                                ;; :UNIQUE-REDIRECT-URI - redirect uri will be unique for every user.
                                                                ;; :PASS-STATE-STRING -  the unique string will be passed to server through go-to-provider route.
                                                                ;;                       It must then match the resulting redirect uri 'state' parameter.
;  (break "in new-oauth-provider")
  
  (let* ((provider         (string-upcase name))
         (provider-low     (string-downcase name))
         (provider-kw      (intern provider 'keyword))
         (provider-module  (intern (concatenate 'string provider +module-str+)         :saluto))
         (provider-var     (intern (concatenate 'string "*" provider +module-str+ "*") :saluto))
         (s-route-go       (intern (concatenate 'string provider ".GO-TO-PROVIDER")    :saluto))
         (s-route-receiver (intern (concatenate 'string provider ".RECEIVER")          :saluto))
         (route-go         (concatenate 'string "/auth/go/" provider-low "/"))
         (route-receiver   (format nil "/auth/receiver/~a/~:[~;:session/~]" provider-low (eq anti-csrf :unique-redirect-uri))))

    `(progn
       (defclass ,provider-module (oauth-2.0-module)())
       (defvar ,provider-var nil)
       (export ',provider-var)
       (export ',provider-module)
       (export ',s-route-go)
       (export ',s-route-receiver)
       (export 'auth.logout)
       (export 'auth.logout/)
       (export 'attach-routes)

       (push ,provider-kw *providers*)

       (defmethod attach-routes ((module ,provider-module))

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
                     ,(case anti-csrf
                            (:unique-redirect-uri
                             'session)
                            (:pass-state-string
                             '(hunchentoot:parameter "state"))
                            (otherwise nil))
                     (hunchentoot:parameter "code")
                     (hunchentoot:parameter "error")))

         (setf (documentation ',s-route-receiver 'function)
               (format nil
                       "This RESTAS route function receive authorization answer from provider '~A'."
                       ,name))

         (restas:define-route auth.logout ("/auth/logout"
                                           :method :get
                                           :content-type "text/html")
           (logout)
           (redirect "/"))
         (restas:define-route auth.logout/ ("/auth/logout/"
                                            :method :get
                                            :content-type "text/html")
           (redirect "/auth/logout"))
;         (break "out attach-routes")
         t)
       
       (defmethod init-module ((module ,provider-module))
         (dolist (i ,init-values)
           (setf (slot-value module (car i))
                 (cdr i)))
         ;; receiver path is constructed at build-goto-path
         (setf (slot-value module 'receiver-path)
               (concatenate 'string "/auth/receiver/" ,provider-low
                            ,(if (eq anti-csrf :unique-redirect-uri)
                                 "/~A/"
                                 "/~*"))))

       (defmethod prepare-userinfo-request ((module ,provider-module) params)
         (let ((fn ,prepare-userinfo-fun))
           (funcall fn module params)))

       (defmethod parse-userinfo ((module ,provider-module) answer)
         (let ((fn ,parse-userinfo-fun))
           (funcall fn module answer)))

       (defmethod go-to-provider ((module ,provider-module))
;         (break "in go-to-provider")
         (let ((fn ,goto-fun))
           (funcall fn module)))

       (defmethod receiver ((module ,provider-module) session code error?)
         (let ((fn ,receiver-fun))
           (funcall fn module session code error?)))

       (defmethod build-goto-path ((module ,provider-module) session-str)
         (assert (slot-value module 'domain))
         (assert (slot-value module 'app-id))
         (assert (or (slot-value module 'app-secret-key)
                     (slot-value module 'app-private-key)))
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
           
           ,(when (eq anti-csrf :pass-state-string)
                  '(setf res (concatenate 'string res
                                          "&" "state=" session-str)))

           (setf res (concatenate 'string res
                                  "&" "redirect_uri="
                                  (full-receiver-path module session-str)))
           res)))))

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
    (push (cons "redirect_uri"  (full-receiver-path module (session))) parameters)
    (push (cons "code" code)                                           parameters)

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

(defmethod full-receiver-path ((module oauth-2.0-module) session-str)
  
  (let ((uri (concatenate 'string
                          (slot-value module 'domain)
                          (format nil
                                  (slot-value module 'receiver-path)
                                  session-str))))
    (if (slot-value module 'encode-uri)
        (drakma:url-encode uri :latin-1)
        uri)))

(defmethod store-userinfo (oauth-2.0-module userinfo)
  (let ((fn (slot-value oauth-2.0-module 'store-userinfo-fun)))
    (when fn
      (funcall fn userinfo))))
