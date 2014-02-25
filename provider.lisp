(in-package #:saluto)

(defvar *main* "/" "Route to the parent site root")

(defclass oauth2-provider ()
  ((app-id
    :reader app-id
    :initarg :app-id)
   (app-private-key
    :reader app-private-key
    :initarg :app-private-key)
   (oauth-login-url
    :reader oauth-login-url)
   (access-token-query-url
    :reader access-token-query-url)
   (userinfo-query-url
    :reader userinfo-query-url)
   (name
    :documentation
    "The name of the provider, used in the provider routes."
    :reader name
    :initarg :name)))

;;;; Generics ;;;;

;;;; First request to provider
;;;; User allows app to read her data

(defgeneric build-goto-path (provider session redirect-uri)
  (:documentation
   "Makes a list of baseurl and parameters
to be converted to a real url."))

(defgeneric receive (provider state code error?)
  (:documentation
   "Receives answer from provider,
asks it for access-token, for data, and redirects to target page"))

;;;; Second request to provider
;;;; The app receives access token

(defgeneric get-access-token (provider code goto-path)
  (:documentation "Retrieves access token, using received code."))

(defgeneric prepare-access-token-request (provider code goto-path)
  (:documentation "DRAKMA:REQUEST is called on result of this function"))

(defgeneric extract-access-token (provider answer)
  (:documentation
   "Extracts access token from the provider answer"))

;;;; Third request to provider
;;;; The app receives userinfo

(defgeneric get-userinfo (provider access-token)
  (:documentation
   "Retrieves user info, using access token"))

(defgeneric prepare-userinfo-request (provider access-token)
  (:documentation "DRAKMA:REQUEST is called on result of this function"))

(defgeneric extract-userinfo (provider request-result)
  (:documentation
   "Extracts user info from the provider answer"))

;;;; Common part of implementation ;;;;

;;;; First request to provider
;;;; User allows app to read her data

(defun make-goto-path (provider session redirect-uri)
  "Converts list to real link"
  (apply #'list-to-link (build-goto-path provider session redirect-uri)))

(defmethod build-goto-path ((provider oauth2-provider) session redirect-uri)
  (list (oauth-login-url provider)
        "client_id" (app-id provider)
        "redirect_uri" (make-redirect-uri provider session redirect-uri)))

(defmethod receive ((provider oauth2-provider) state code error?)
  (destructuring-bind (state redirect-uri) (split-state state)
    (if (invalid-receiver-params? code
                                  state
                                  error?)
        (progn (logout)
               (redirect *main*))
        (progn
          (get-userinfo
           provider
           (get-access-token provider
                             code
                             (make-redirect-uri provider
                                                state
                                                redirect-uri)))
          (redirect redirect-uri)))))

;;;; Second request to provider
;;;; The app receives access token

(defmethod get-access-token ((provider oauth2-provider) code goto-path)
  (extract-access-token
   provider
   (request (prepare-access-token-request provider code goto-path))))

(defmethod prepare-access-token-request ((provider oauth2-provider)
                                         code
                                         goto-path)
    (list (access-token-query-url provider)
          :method :POST
          :content-length t
          :parameters
          `(("client_id"     . ,(app-id provider))
            ("client_secret" . ,(app-private-key provider))
            ("redirect_uri"  . ,goto-path)
            ("code"          . ,code))))

(defmethod extract-access-token ((provider oauth2-provider) answer)
  (jsown:val (jsown:parse answer) "access_token"))

;;;; Third request to provider
;;;; The app receives userinfo

(defvar *store-userinfo-fun*
  (lambda (userinfo)
    (declare (ignore userinfo))
    (error "SALUTO: userinfo storing not implemented"))
  "To be replaced on RESTAS:MOUNT-MODULE")

(defun store-userinfo (userinfo)
  (funcall *store-userinfo-fun* userinfo))

(defmethod get-userinfo ((provider oauth2-provider) access-token)
  (store-userinfo
   (extract-userinfo
    provider
    (request (prepare-userinfo-request provider access-token)))))

(defmethod prepare-userinfo-request ((provider oauth2-provider) access-token)
  (list (userinfo-query-url provider)
        :parameters (list (cons "access_token" access-token))
        :content-length t
        :method :get))

(defmethod extract-userinfo :around ((provider oauth2-provider) answer)
  (append (list :session (session)
                :provider (name provider))
          (call-next-method provider (jsown:parse answer))))
