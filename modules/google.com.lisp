(in-package #:saluto)

(eval-when (:load-toplevel)
  (new-oauth-provider "GOOGLE.COM"

                      :anti-csrf :pass-state-string

;;; ==================================================================

                      :init-values '((provider-name . "Google.com")
                                     (oauth-host . "")
                                     (api-host . "https://www.googleapis.com")
                                     (post-access-param-via . :DATA)
                                     (access-token-method . :POST)
                                     (encode-uri . t)
                                     (query-params . (("response_type" . "code")
                                                      ("scope" . "https://www.googleapis.com/auth/userinfo.email+https://www.googleapis.com/auth/userinfo.profile")))
                                     (oauth-path . "https://accounts.google.com/o/oauth2/auth")
                                     (token-params . (("grant_type" . "authorization_code")))
                                     (access-token-path . "https://accounts.google.com/o/oauth2/token"))

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

                        (let ((access-token (extract-access-token
                                             (remove-zeros-from-string
                                              (request (prepare-access-token-request module code)))))
                              (userinfo-request nil)
                              (userinfo nil))

                          (setf userinfo-request
                                (prepare-userinfo-request module access-token))
                          (setf userinfo (remove-zeros-from-string (request userinfo-request)))
                          (let ((parsed-userinfo (parse-userinfo module userinfo)))
                            (store-userinfo module parsed-userinfo)))
                        (redirect "/"))

;;; ==================================================================

                      :prepare-userinfo-fun
                      (alexandria:named-lambda prepare-user-info-fun (module access-token)
                        (list (concatenate 'string (api-host module) "/oauth2/v1/userinfo")
                              :parameters (list (cons "access_token" access-token))
                              :content-length t
                              :method :get))

;;; ==================================================================

                      :parse-userinfo-fun
                      (alexandria:named-lambda parse-userinfo-fun (module answer)
                        (let* ((parsed-answer (jsown:parse answer))

                               (first-name
                                 (sb-ext:octets-to-string
                                  (sb-ext:string-to-octets
                                   (json-val parsed-answer "given_name")
                                   :EXTERNAL-FORMAT :LATIN-1)
                                  :EXTERNAL-FORMAT :UTF-8))

                               (last-name
                                 (sb-ext:octets-to-string
                                  (sb-ext:string-to-octets
                                   (json-val parsed-answer "family_name")
                                   :EXTERNAL-FORMAT :LATIN-1)
                                  :EXTERNAL-FORMAT :UTF-8))

                               (avatar (json-val parsed-answer "picture"))
                               (email (json-val parsed-answer "email"))
                               (uid (json-val parsed-answer "id")))
                          
                          (list :first-name first-name
                                :last-name last-name
                                :avatar avatar
                                :email email
                                :uid uid
                                :session (session)
                                :provider "Google.com")))))
