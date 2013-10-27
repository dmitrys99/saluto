(restas:define-module #:saluto
  (:use #:cl)
  (:export #:*main*
           #:*store-userinfo-fun*
           #:*providers*
           #:oauth2-google.com
           #:oauth2-facebook.com
           #:oauth2-mail.ru
           #:oauth2-vk.com))