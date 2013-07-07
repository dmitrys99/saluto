(ql:quickload '("cl-who" "restas" "saluto"))

(restas:define-module #:restas.test-saluto
  (:use #:cl))

(in-package #:restas.test-saluto)

(defvar *users* (make-hash-table :test #'equal))

(saluto:attach-saluto (list (list :module :facebook.com
                                  :app-id "<facebook-app-id>"
                                  :app-private "<facebook-app-private>"
                                  :domain "http://localhost:8080")
                            (list :module :mail.ru
                                  :app-id "<mailru-app-id>"
                                  :app-private "<mailru-app-private>"
                                  :app-secret "<mailru-app-secret>"
                                  :domain "http://localhost:8080")
                            (list :module :google.com
                                  :app-id "<google-app-id>"
                                  :app-private "<google-app-private>"
                                  :domain "http://localhost:8080"))
                      (lambda (info)
                        (setf (gethash hunchentoot:*session* *users*) info)))

(restas:mount-module saluto (#:saluto)
  (:inherit-parent-context t))

(restas:define-route main ("" :method :get)
  (who:with-html-output-to-string (out)
    (:html
     (:head (:title "Testing saluto"))
     (:body
      (:h1 "Testing saluto")
      (if (gethash hunchentoot:*session* *users* nil)
          (let ((slots (gethash hunchentoot:*session* *users*)))
            (who:htm
             (:div (:img :src (getf slots :avatar) :style "float: left; padding-right: 10px;")
                   (:p (who:esc (format nil "~a ~a" (getf slots :last-name) (getf slots :first-name))))
                   (:p (:a :href (restas:genurl 'saluto.auth.logout) "Logout")))))
          (who:htm 
           (:p (:a :href (restas:genurl 'saluto.facebook.com.go-to-provider)
                   "Login with FACEBOOK.COM"))
           (:p (:a :href (restas:genurl 'saluto.mail.ru.go-to-provider)
                   "Login with MAIL.RU"))
           (:p (:a :href (restas:genurl 'saluto.google.com.go-to-provider)
                   "Login with GOOGLE.COM"))
           (:p "Not logged in")))))))

(restas:define-route receiver.mail.ru ("receiver.html")
  "<html>
<body>
<script src=\"http://connect.mail.ru/js/loader.js\"></script>
<script>
mailru.loader.require('receiver', function(){
	mailru.receiver.init();
})
</script>
</body>
</html> 
")

(restas:start '#:restas.test-saluto :port 8080)
