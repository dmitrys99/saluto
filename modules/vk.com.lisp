(in-package #:saluto)

(eval-when (:load-toplevel)
  (new-oauth-provider "VK.COM"

;;; ==================================================================

                      :init-values '((provider-name           . "vk.com")
                                     (oauth-host              . "https://oauth.vk.com")
                                     (oauth-path              . "/authorize")
                                     (api-host                . "https://api.vk.com/method/")
                                     (query-params            . (("response_type"  . "code")
                                                                 #|("scope"  . "photos,notify")|#))
                                     (token-params            . ())
                                     (access-token-path       . "/access_token"))

;;; ==================================================================

                      :goto-fun 'goto-fun

;;; ==================================================================

                      :receiver-fun 'rec-fun
                      
;;; ==================================================================

                      :prepare-userinfo-fun 'prepare-userinfo-fun

;;; ==================================================================

                      :parse-userinfo-fun 'parse-userinfo-fun))


(defun goto-fun (module)
  (if (not (session))
      (progn
        (start-session)
        (redirect (build-goto-path module (session))))
      (redirect "/")))


(defun rec-fun (module session code error?)
  (when (invalid-receiver-params? code
                                  session
                                  error?)
    (logout)
    (redirect "/"))
  
  (let* ((rq (request (prepare-access-token-request module code)))
         (access-token (extract-access-token
                        (sb-ext:octets-to-string rq)))
         (userinfo-request nil)
         (userinfo nil))
    ;; (break "~A" rq)
    (setf userinfo-request
          (prepare-userinfo-request module access-token))
    (setf userinfo (request userinfo-request))
    (let ((parsed-userinfo (parse-userinfo module userinfo)))
      (store-userinfo module parsed-userinfo)))
  (redirect "/"))


(defun parse-userinfo-fun (module answer)
  (let* ((decoded (sb-ext:octets-to-string answer))
         (parsed-answer (first (json-val (jsown:parse decoded) "response")))
         (first-name (json-val parsed-answer "first_name"))
         (last-name  (json-val parsed-answer "last_name"))
         (avatar     (json-val parsed-answer "photo_max")) ;; may be just "photo"
         ;; (email      (jsown:val parsed-answer "email"))
         (uid        (write-to-string (json-val parsed-answer "uid"))))
;    (break "decoded: ~A" decoded)
;    (break "parsed-answer: ~A" parsed-answer)    
    (list :first-name first-name
          :last-name  last-name
          :avatar     avatar
          :uid        uid
          :session    (session)
          :provider   "vk.com")))

(defun prepare-userinfo-fun (module auth-key)
  (list (concatenate 'string (api-host module) "users.get")
        :parameters (list (cons "fields" "photo_max")
                          (cons "access_token" (first auth-key))
                          (cons "uids" (write-to-string (second auth-key))))
        :content-length t
        :method :get))
