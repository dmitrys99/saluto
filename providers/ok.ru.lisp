(in-package #:saluto)

(defclass oauth2-ok.ru (oauth2-provider)
  ((oauth-login-url
    :initform "http://www.odnoklassniki.ru/oauth/authorize"
    :allocation :class)
   (access-token-query-url
    :initform "http://api.odnoklassniki.ru/oauth/token.do"
    :allocation :class)
   (userinfo-query-url
    :initform "http://api.odnoklassniki.ru/fb.do"
    :allocation :class)))

(defmethod make-redirect-uri ((provider oauth2-ok.ru) session redirect-uri)
  (declare (ignore session redirect-uri))
  (restas:genurl* 'receiver-route
                  :provider (name provider)
                  :states ""))

(defmethod build-goto-path :around ((provider oauth2-ok.ru)
                                    session
                                    redirect-uri)
  (append
   (call-next-method provider session redirect-uri)
   (list "response_type" "code"
         "state" (make-state session redirect-uri))
   ))

(defmethod prepare-access-token-request :around ((provider
                                                  oauth2-ok.ru)
                                                 code
                                                 goto-path)
  "Ok.Ru needs parameters to be send as data"
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

(defun make-signature (provider access-token)
  (let ((sgn
	  (format nil "application_key=~Amethod=users.getCurrentUser~A"
		  (app-public-key provider)
		  (md5 (format nil "~A~A" access-token (app-private-key provider))))))
#+SALUTO-DEBUG    (break "~S" sgn)
    (md5 sgn)))

(defmethod prepare-userinfo-request ((provider oauth2-ok.ru)
                                     access-token)
  (let ((params (list (cons "access_token" access-token)
		      (cons "application_key" (app-public-key provider))
                      (cons "method" "users.getCurrentUser"))))
    (list (userinfo-query-url provider)
          :parameters (cons
                       (cons "sig" (make-signature provider access-token))
                       params)
          :content-length t
          :method :get)))

(defmethod extract-access-token :around ((provider oauth2-ok.ru) answer)
  (call-next-method provider (babel:octets-to-string answer :encoding :UTF-8)))

(defmethod extract-userinfo :around ((provider oauth2-ok.ru) answer)
  (call-next-method provider (babel:octets-to-string answer :encoding :UTF-8)))

(defmethod extract-userinfo ((provider oauth2-ok.ru)
                             parsed-answer)
#+SALUTO-DEBUG  (break "Parsed answer ~A" parsed-answer)
  (list :first-name (json-val parsed-answer "first_name")
        :last-name (json-val parsed-answer "last_name")
        :avatar (json-val parsed-answer "pic_1")
        :email ""
        :uid (json-val parsed-answer "uid")))
