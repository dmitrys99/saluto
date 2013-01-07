(in-package #:saluto)

(new-oauth-provider "MAIL.RU")

(defmethod init-module (mail.ru-module)
  (let ((init '((provider-name . "Mail.Ru")
                (oauth-host . "https://connect.mail.ru")
                (api-host . "http://www.appsmail.ru/platform/api")
                (access-token-path . "/oauth/token"))))
    (dolist (i init)
      (setf (slot-value mail.ru-module (car i))
            (cdr i)))))
