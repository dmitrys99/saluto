(restas:define-module #:saluto
  (:use #:cl)
  (:export #:*main*
           #:*store-userinfo-fun*
           #:*logged-in-p-fun*
           #:*providers*
           #:oauth2-facebook.com
           #:oauth2-github.com
           #:oauth2-google.com
           #:oauth2-mail.ru
           #:oauth2-vk.com))