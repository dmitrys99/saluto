Saluto
======

Authentication (OAuth 2.0) module for RESTAS

Saluto is a submodule for RESTAS web-framework, which allow web-site users to be authorized with OAuth 2.0 protocol.

Source code: <https://github.com/dmitrys99/saluto>

Currently implemented OAuth 2.0 providers:

* Facebook
* VK.com
* OK.ru
* Github.com
* Google.com
* Mail.ru

Example usage:
-------------

<pre><code>
(defun do-init-site ()
  (init-cache)
  (init-db)

  (restas:mount-module saluto (#:saluto)
                       (:url "auth/")
                       (:inherit-parent-context t)
                       (saluto:*providers* (list
                                            (make-instance 'saluto:oauth2-facebook.com
                                                           :name "facebook.com"
                                                           :app-id "<app-id>"
                                                           :app-private-key "<app-key>")
                                            (make-instance 'saluto:oauth2-vk.com
                                                           :name "vk.com"
                                                           :app-id "<app-id>"
                                                           :app-private-key "<app-key>")
                                            (make-instance 'saluto:oauth2-ok.ru
                                                           :name "odnoklassniki.ru"
                                                           :app-id "<app-id>"
                                                           :app-public-key "<app-key>"
                                                           :app-private-key "<app-private>")))
                       (saluto:*store-userinfo-fun*
                        (lambda (info)
                          (when info
                            ;; Process user info, returned by OAuth provider, like this:
                            ;; (maybe-save-user info :cache-user t :session (session-identifier))
                            ;;
                            ;; Structure of user info:
                            ;; '(:first-name <first name>
                            ;;   :last-name <last-name>
                            ;;   :email <email>
                            ;;   :uid <user id at provider level>
                            ;;   :avatar <user avatar URI>
                            ;;   :provider <provider name>)
                            )))
                       (saluto:*logged-in-p-fun*
                        ;; function, called to check if user logged or not
                        (lambda ()))
                       (saluto:*logout-fun*
                        ;; function, called on logout
                        (lambda (session)
                          ;;
                          ;; (delete-userauth-from-cache session)
                          ))))
</code></pre>

To start login process point user to URI /auth/goto/provider, ex.: <http://localhost:8080/auth/goto/vk.com/>

For full example usage see `example.lisp`.
