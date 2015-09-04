(in-package #:saluto)

(defclass oauth2-github.com (oauth2-provider)
  ((oauth-login-url
    :initform "https://github.com/login/oauth/authorize"
    :allocation :class)
   (access-token-query-url
    :initform "https://github.com/login/oauth/access_token"
    :allocation :class)
   (userinfo-query-url
    :initform "https://api.github.com/user"
    :allocation :class)))

(defmethod make-redirect-uri ((provider oauth2-github.com) session redirect-uri)
  (declare (ignore session redirect-uri))
  (restas:genurl* 'receiver-route
                  :provider (name provider)
                  :states ""))

(defmethod build-goto-path :around ((provider oauth2-github.com)
                                    session
                                    redirect-uri)
  (append
   (call-next-method provider session redirect-uri)
   (list
    "response_type" "code"
    "scope" "user:email"
    "state" (make-state session redirect-uri))))

(defmethod prepare-access-token-request :around ((provider
                                                  oauth2-github.com)
                                                 code
                                                 goto-path)
  (let ((request (call-next-method provider code goto-path)))
    (append request '(:ACCEPT "application/json"))))

(defmethod extract-access-token :around ((provider oauth2-github.com)
                                         answer)
  (call-next-method provider (babel:octets-to-string answer :encoding :UTF-8)))

(defmethod extract-userinfo :around ((provider oauth2-github.com)
                                     answer)
  (call-next-method provider (babel:octets-to-string answer :encoding :UTF-8)))

(defmethod extract-userinfo ((provider oauth2-github.com)
                             parsed-answer)
  (list :first-name (or (json-val parsed-answer "name")
                        (json-val parsed-answer "login"))
        :last-name ""
        :avatar (json-val parsed-answer "avatar_url")
        :email (json-val parsed-answer "email")
        :uid (json-val parsed-answer "id")))

