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
                        (parse-userinfo module userinfo))
                      (redirect "/"))

;;; ==================================================================

                    :prepare-userinfo-fun
                    (lambda (module params)
                      (let* ((parameters (sort-params params))
                             (params-reverse (reverse parameters))
                             (con-params (concatenate-params parameters :delimiter nil))
                             (sig "")
                             (2sig (concatenate 'string
                                                con-params
                                                (app-secret module))))
                        ;; (break "con-params: ~A~%secret: ~A~%2sig: ~A" con-params (app-secret module) 2sig)
                        (setf sig (md5 2sig))
                        (push (cons "sig" sig) params-reverse)
                        ;;(break "~A" (reverse params-reverse))
                        (list (api-host module)
                              :parameters (reverse params-reverse)
                              :content-length t
                              :method :get)))

;;; ==================================================================

                    :parse-userinfo-fun
                    (lambda (module answer)
                      (break "answer: ~A" answer)
                      ))
