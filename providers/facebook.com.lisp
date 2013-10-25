(in-package #:saluto)

(defclass oauth2-facebook.com (oauth2-provider)
  ((oauth-login-url
    :initform "https://www.facebook.com/dialog/oauth"
    :allocation :class)
   (access-token-query-url
    :initform "https://graph.facebook.com/oauth/access_token"
    :allocation :class)
   (userinfo-query-url
    :initform "https://graph.facebook.com/me"
    :allocation :class)))

(defmethod make-redirect-uri ((provider oauth2-facebook.com) session redirect-uri)
  (declare (ignore session redirect-uri))
  (restas:genurl* 'receiver-route
                  :provider (name provider)
                  :states ""))

(defmethod build-goto-path :around ((provider oauth2-facebook.com)
                                    session
                                    redirect-uri)
  (append
   (call-next-method provider session redirect-uri)
   (list "scope" "email"
         "state" (make-state session redirect-uri))))

(defmethod prepare-userinfo-request :around ((provider oauth2-facebook.com)
                                             access-token)
  (let ((request (call-next-method provider access-token)))
    (setf (getf (cdr request) :parameters)
          (cons (cons "fields" "picture,id,first_name,last_name,email")
                (getf (cdr request) :parameters)))
    request))

(defmethod extract-access-token ((provider oauth2-facebook.com) answer)
  (second
   (split-sequence:split-sequence
    #\=
    (car
     (split-sequence:SPLIT-SEQUENCE #\& answer)))))

(defmethod extract-userinfo :around ((provider oauth2-facebook.com)
                                     answer)
  (call-next-method provider (sb-ext:octets-to-string answer :external-format :UTF-8)))

(defmethod extract-userinfo ((provider oauth2-facebook.com)
                             parsed-answer)
  (list :first-name (json-val parsed-answer "first_name")
        :last-name (json-val parsed-answer "last_name")
        :avatar (json-val
                 (json-val
                  (json-val
                   parsed-answer
                   "picture")
                  "data")
                 "url")
        :email (json-val parsed-answer "email")
        :uid (json-val parsed-answer "id")))
