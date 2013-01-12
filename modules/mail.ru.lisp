(in-package #:saluto)

(new-oauth-provider "MAIL.RU"
                    :init-values '((provider-name . "Mail.Ru")
                                   (oauth-host . "https://connect.mail.ru")
                                   (api-host . "http://www.appsmail.ru/platform/api")
                                   (post-access-param-via . :DATA)
                                   (access-token-method . :POST)
                                   (query-params . (("response_type" . "code")))
                                   (access-token-path . "/oauth/token"))

                    :goto-fun     (lambda (module)
                                    (if (not (session))
                                        (progn
                                          (start-session)
                                          (redirect (build-goto-path module (session))))
                                        (redirect "/")))

                    :receiver-fun (lambda (module session code error?)
                                    (declare (ignore session code error?))
                                    (break "mail.ru receiver")
                                    (slot-value module 'oauth-host)))

