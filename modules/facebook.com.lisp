(in-package #:saluto)

(eval-when (:load-toplevel :execute)
  (new-oauth-provider "FACEBOOK.COM"

;;; ==================================================================

                      :init-values '((provider-name           . "facebook.com")
                                     (oauth-host              . "")
                                     ;;; There are two different hosts
                                     ;;; for OAuth requests on facebook.com
                                     ;;; That's why hosts are included in paths
                                     (oauth-path              . "https://www.facebook.com/dialog/oauth")
                                     (query-params            . (("scope"  . "email")))
                                     (access-token-path       . "https://graph.facebook.com/oauth/access_token")
                                     (token-params            . ())
                                     (encode-uri              . nil)
                                     (api-host                . "https://graph.facebook.com/"))

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

                        (let* ((rq (request (prepare-access-token-request module code)))
                               (access-token
                                 (second
                                  (split-sequence:split-sequence
                                   #\=
                                   (car
                                    (split-sequence:SPLIT-SEQUENCE #\& rq)))))
                               (userinfo-request nil)
                               (userinfo nil))
                          (setf userinfo-request
                                (prepare-userinfo-request module access-token))
                          (setf userinfo (request userinfo-request))
                          (let ((parsed-userinfo (parse-userinfo module userinfo)))
                            (store-userinfo module parsed-userinfo)))
                        (redirect "/"))

;;; ==================================================================

                      :prepare-userinfo-fun
                      (alexandria:named-lambda prepare-user-info-fun (module access-token)
                        (list (concatenate 'string (api-host module) "me")
                              :parameters (list (cons "access_token" access-token)
                                                (cons "fields" "picture,id,first_name,last_name,email"))
                              :content-length t
                              :method :get))

;;; ==================================================================

                      :parse-userinfo-fun
                      (alexandria:named-lambda parse-userinfo-fun (module answer)
                        (declare (ignore module))
                        (let* ((parsed-answer (jsown:parse (sb-ext:octets-to-string answer)))
                               (first-name (json-val parsed-answer "first_name"))
                               (last-name  (json-val parsed-answer "last_name"))
                               (avatar     (json-val
                                            (json-val
                                             (json-val
                                              parsed-answer
                                              "picture")
                                             "data")
                                            "url"))
                               (email      (json-val parsed-answer "email"))
                               (uid        (json-val parsed-answer "id")))
                          (list :first-name first-name
                                :last-name  last-name
                                :avatar     avatar
                                :email      email
                                :uid        uid
                                :session    (session)
                                :provider   "facebook.com")))))
