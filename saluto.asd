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
               (:file "utils")
               (:file "provider")
               (:file "routes")
               (:module "providers"
                :components ((:file "google.com")
                             (:file "facebook.com")
                             (:file "mail.ru")
                             (:file "vk.com")))))
