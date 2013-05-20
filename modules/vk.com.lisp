(in-package #:saluto)

#|
(eval-when (:load-toplevel)
  (new-oauth-provider "VK.COM"

;;; ==================================================================

                      :init-values '((provider-name           . "Vk.com")
                                     (oauth-host              . "https://oauth.vk.com/")
                                     (api-host                . "http://www.appsmail.ru/platform/api")
                                     ;(post-access-param-via   . )
                                     (access-token-method     . :GET)
                                     (query-params            . (("response_type"  . "code")))
                                     (token-params            . (("grant_type"     . "authorization_code")))
                                     (access-token-path       . "/oauth/token"))

;;; ==================================================================

                      :goto-fun
                      (alexandria:named-lambda goto-fun (module)
                        (if (not (session))
                            (progn
                              (start-session)
                              (redirect (build-goto-path module (session))))
                            (redirect "/")))

;;; ==================================================================

                      :receiver-fun
                      (alexandria:named-lambda receiver-fun (module session code error?)
                        (when (invalid-receiver-params? code
                                                        session
                                                        error?)
                          (logout)
                          (redirect "/"))

                        (let ((auth-key (extract-authorization-key
                                         (request (prepare-access-token-request module code))))
                              (userinfo-request nil)
                              (userinfo nil))

                          (setf userinfo-request
                                (prepare-userinfo-request module (list (cons "app_id"      (app-id module))
                                                                       (cons "method"      "users.getInfo")
                                                                       (cons "secure"      "1")
                                                                       (cons "session_key" auth-key))))
                          (setf userinfo (request userinfo-request))
                          (let ((parsed-userinfo (parse-userinfo module userinfo)))
                            (store-userinfo module parsed-userinfo)))
                        (redirect "/"))

;;; ==================================================================

                      :prepare-userinfo-fun
                      (alexandria:named-lambda prepare-user-info-fun (module params)
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
                          ;; (break "~A" (reverse params-reverse))
                          (list (api-host module)
                                :parameters (reverse params-reverse)
                                :content-length t
                                :method :get)))

;;; ==================================================================

                      :parse-userinfo-fun
                      (alexandria:named-lambda parse-userinfo-fun (module answer)
                        (let* ((parsed-answer (first (jsown:parse answer)))
                               (first-name (json-val parsed-answer "first_name"))
                               (last-name  (json-val parsed-answer "last_name"))
                               (avatar     (json-val parsed-answer "pic_22"))
                               (email      (json-val parsed-answer "email"))
                               (uid        (json-val parsed-answer "uid")))
                          (list :first-name first-name
                                :last-name  last-name
                                :avatar     avatar
                                :email      email
                                :uid        uid
                                :session    (session)
                                :provider   "Mail.Ru")))))
|#
