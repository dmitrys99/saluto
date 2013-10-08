; -*- coding: utf-8 mode: lisp -*-

(defsystem saluto
  :name "Saluto"
  :author "Dmitry Solomennikov <dmitrys99@mail.ru>"
  :version "0.0.1"
  :description "OAuth 2.0 authentication for RESTAS"
  :depends-on (#:hunchentoot
               #:restas
               #:ironclad
               #:split-sequence
               #:jsown
               #:cl-ppcre
               #:drakma)
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:file "config")
                             (:file "base")
                             (:file "utils")
                             (:file "oauth")
                             (:file "code")))
               (:module "modules"
                :serial t
                :components ((:file "mail.ru")
                             (:file "vk.com")
                             (:file "google.com")
                             (:file "facebook.com")
                             (:file "twitter.com")))))

