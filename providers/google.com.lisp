(in-package #:saluto)

(defclass oauth2-google.com (oauth2-provider)
  ((oauth-login-url
    :initform "https://accounts.google.com/o/oauth2/auth"
    :allocation :class)
   (access-token-query-url
    :initform "https://accounts.google.com/o/oauth2/token"
    :allocation :class)
   (userinfo-query-url
    :initform "https://www.googleapis.com/oauth2/v1/userinfo"
    :allocation :class)))

(defmethod make-redirect-uri ((provider oauth2-google.com) session redirect-uri)
  (declare (ignore session redirect-uri))
  (restas:genurl* 'receiver-route
                  :provider (name provider)
                  :states ""))

(defmethod build-goto-path :around ((provider oauth2-google.com)
                                    session
                                    redirect-uri)
  (append
   (call-next-method provider session redirect-uri)
   (list
    "response_type" "code"
    "scope" "https://www.googleapis.com/auth/userinfo.email+https://www.googleapis.com/auth/userinfo.profile"
    "state" (make-state session redirect-uri))))

(defmethod prepare-access-token-request :around ((provider
                                                  oauth2-google.com)
                                                 access-token
                                                 goto-path)
  "Google needs parameters to be send as data"
  (let ((request (call-next-method provider access-token goto-path)))
    (setf (getf (cdr request) :parameters)
          (concatenate-params (cons
                               '("grant_type" "authorization_code")
                               (getf (cdr request) :parameters))))
    (substitute :content :parameters request)))

(defun remove-zeros-from-string (array)
  "This function is needed by google.com provider,
because the answer of google.com for unknown reasons contains sudden chunks of zeros."
  (coerce (loop for x across array unless (zerop x)
               collect (code-char x))
          'string))

(defmethod extract-access-token :around ((provider oauth2-google.com)
                                         answer)
  (call-next-method provider (remove-zeros-from-string answer)))

(defmethod extract-userinfo :around ((provider oauth2-google.com)
                                     answer)
  (call-next-method provider (remove-zeros-from-string answer)))

(defmethod extract-userinfo ((provider oauth2-google.com)
                             parsed-answer)
  (labels ((code-decode (string)              ;;;; Indeed, I don't know what does it mean
             (sb-ext:octets-to-string
              (sb-ext:string-to-octets
               string
               :EXTERNAL-FORMAT :LATIN-1)
              :EXTERNAL-FORMAT :UTF-8)))
    (list :first-name (code-decode (json-val parsed-answer "given_name"))
          :last-name (code-decode (json-val parsed-answer "family_name"))
          :avatar (json-val parsed-answer "picture")
          :email (json-val parsed-answer "email")
          :uid (json-val parsed-answer "id"))))

