(in-package #:saluto)

(new-oauth-provider "MAIL.RU"
                    :init-values '((provider-name . "Mail.Ru")
                                   (oauth-host . "https://connect.mail.ru")
                                   (api-host . "http://www.appsmail.ru/platform/api")
                                   (post-access-param-via . :DATA)
                                   (access-token-method . :POST)
                                   (query-params . (("response_type" . "code")))
                                   (token-params . (("grant_type" . "authorization_code")))
                                   (access-token-path . "/oauth/token"))

                    :goto-fun
                    (lambda (module)
                      (if (not (session))
                          (progn
                            (start-session)
                            (redirect (build-goto-path module (session))))
                          (redirect "/")))


                    :receiver-fun
                    (lambda (module session code error?)
                      (when (invalid-receiver-params? code
                                                      session
                                                      error?)
                        (logout)
                        (redirect "/"))
                      )

                    )
