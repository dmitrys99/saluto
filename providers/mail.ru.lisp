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

(defun md5 (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5
                             (babel:string-to-octets str))))

(defun make-signature (provider params)
  (md5 (format nil "~{~{~a=~a~}~}~a"
               (mapcar (lambda (x)
                         (list (car x) (cdr x)))
                       params)
               (app-private-key provider))))

(defmethod prepare-userinfo-request ((provider
                                      oauth2-mail.ru)
                                     access-token)
  (let ((params (list (cons "app_id" (app-id provider))
                      (cons "method" "users.getInfo")
                      (cons "secure" "1")
                      (cons "session_key" access-token))))
    (list (userinfo-query-url provider)
          :parameters (cons
                       (cons "sig" (make-signature provider params))
                       params)
          :content-length t
          :method :get)))

(defmethod extract-userinfo ((provider oauth2-mail.ru)
                             parsed-answer)
  (setf parsed-answer (car parsed-answer))
  (list :first-name (json-val parsed-answer "first_name")
        :last-name (json-val parsed-answer "last_name")
        :avatar (json-val parsed-answer "pic_50")
        :email (json-val parsed-answer "email")
        :uid (json-val parsed-answer "uid")))
