(in-package #:saluto)

(defclass oauth2-vk.com (oauth2-provider)
  ((oauth-login-url
    :initform "https://oauth.vk.com/authorize"
    :allocation :class)
   (access-token-query-url
    :initform "https://oauth.vk.com/access_token"
    :allocation :class)
   (userinfo-query-url
    :initform "https://api.vk.com/method/users.get"
    :allocation :class)))

(defmethod make-redirect-uri ((provider oauth2-vk.com) session redirect-uri)
  (declare (ignore session redirect-uri))
  (restas:genurl* 'receiver-route
                  :provider (name provider)
                  :states ""))

(defmethod build-goto-path :around ((provider oauth2-vk.com)
                                    session
                                    redirect-uri)
  (append
   (call-next-method provider session redirect-uri)
   (list "response_type" "code"
         "state" (make-state session redirect-uri))))

(defmethod prepare-userinfo-request :around ((provider oauth2-vk.com)
                                             access-token)
  (let ((request (call-next-method provider access-token)))
    (setf (getf (cdr request) :parameters)
          (cons (cons "fields" "photo")
                
                (getf (cdr request) :parameters)))
    (substitute :GET :POST request)))

(defmethod extract-access-token :around ((provider oauth2-vk.com)
                                         answer)
  (call-next-method provider (sb-ext:octets-to-string answer :external-format :UTF-8)))

(defmethod extract-userinfo :around ((provider oauth2-vk.com)
                                         answer)
  (call-next-method provider (sb-ext:octets-to-string answer :external-format :UTF-8)))

(defmethod extract-userinfo ((provider oauth2-vk.com)
                             parsed-answer)
  (setf parsed-answer (car (json-val parsed-answer "response")))
  (list :first-name (json-val parsed-answer "first_name")
        :last-name (json-val parsed-answer "last_name")
        :avatar (json-val parsed-answer "photo")
        :email (json-val parsed-answer "email")
        :uid (json-val parsed-answer "id")))
