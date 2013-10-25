(in-package #:saluto)

(defclass oauth2-mail.ru (oauth2-provider)
  ((oauth-login-url
    :initform "https://connect.mail.ru/oauth/authorize"
    :allocation :class)
   (access-token-query-url
    :initform "https://connect.mail.ru/oauth/token"
    :allocation :class)
   (userinfo-query-url
    :initform "http://www.appsmail.ru/platform/api"
    :allocation :class)))

(defmethod make-redirect-uri ((provider oauth2-mail.ru) session redirect-uri)
  (declare (ignore session redirect-uri))
  (restas:genurl* 'receiver-route
                  :provider (name provider)
                  :states (make-state session redirect-uri)))

(defmethod build-goto-path :around ((provider oauth2-mail.ru)
                                    session
                                    redirect-uri)
  (append
   (call-next-method provider session redirect-uri)
   (list "response_type" "code")))

(defmethod prepare-access-token-request :around ((provider
                                                  oauth2-mail.ru)
                                                 code
                                                 goto-path)
  "Mail.Ru needs parameters to be send as data"
  (let ((request (call-next-method provider code goto-path)))
    (setf (getf (cdr request) :parameters)
          (concatenate-params (cons
                               '("grant_type" . "authorization_code")
                               (getf (cdr request) :parameters))))
    (substitute :content :parameters request)))

(defmethod prepare-userinfo-request :around ((provider
                                              oauth2-mail.ru)
                                             access-token)
  (let ((request (call-next-method provider access-token)))
    (setf (getf (cdr request) :parameters)
          (append
           '(("method" . "users.getInfo")
             ("secure" . "1"))
           (getf (cdr request) :parameters)))
    request))

(defmethod extract-userinfo ((provider oauth2-mail.ru)
                             parsed-answer)
  (list :first-name (json-val parsed-answer "first_name")
        :last-name (json-val parsed-answer "last_name")
        :avatar (json-val parsed-answer "pic_22")
        :email (json-val parsed-answer "email")
        :uid (json-val parsed-answer "uid")))
