(in-package #:saluto)

(new-oauth-provider "MAIL.RU"
;;; ==================================================================
                    :init-values '((provider-name . "Mail.Ru")
                                   (oauth-host . "https://connect.mail.ru")
                                   (api-host . "http://www.appsmail.ru/platform/api")
                                   (post-access-param-via . :DATA)
                                   (access-token-method . :POST)
                                   (query-params . (("response_type" . "code")))
                                   (token-params . (("grant_type" . "authorization_code")))
                                   (access-token-path . "/oauth/token"))

;;; ==================================================================
                    :goto-fun
                    (lambda (module)
                      (if (not (session))
                          (progn
                            (start-session)
                            (redirect (build-goto-path module (session))))
                          (redirect "/")))


;;; ==================================================================
                    :receiver-fun
                    (lambda (module session code error?)
                      (when (invalid-receiver-params? code
                                                      session
                                                      error?)
                        (logout)
                        (redirect "/"))

                      (let ((auth-key (extract-authorization-key
                                       (request (prepare-access-token-request module code))))
                            (user-info-request nil)
                            (userinfo nil))

                        (setf user-info-request
                              (prepare-userinfo module (list (cons "app_id" (app-id module))
                                                             (cons "method" "users.getInfo")
                                                             (cons "secure" "1")
                                                             (cons "session_key" auth-key))))
                        (setf userinfo (request user-info-request))
                        (parse-userinfo module userinfo)))

;;; ==================================================================
                      :prepare-userinfo-fun
                      (lambda (module params)
                        (let ((parameters (reverse (sort-params params)))
                              (sig ""))
                          (setf sig (md5 (concatenate 'string
                                                      (concatenate-params params nil)
                                                      (app-secret module))))
                          (push (cons "sig" sig) parameters)
                          (list (api-host module)
                                :parameters parameters
                                :content-length t
                                :method :get)))

                      :parse-userinfo-fun
                      (lambda (module answer)
                      (break "answer: ~A" answer)))
