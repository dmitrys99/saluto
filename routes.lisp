(in-package #:saluto)

(defvar *providers* '()
  "Variable, containing providers objects
Should be replaced when mounting module")

(defun parse-provider (provider-name)
  (or (find provider-name (print *providers*) :key #'name :test #'string=)
      (error "SALUTO: No such provider ~A" provider-name)))

(restas:define-route login-with ("goto/:provider/" :method :get)
;  (:sift-variables (provider #'parse-provider))
  (:additional-variables (redirect-uri (hunchentoot:parameter "redirect")))
  ;; This REDIRECT-URI means just target page after successful login
(let ((provider (parse-provider provider)))
  (if (not (session))
      (progn
        (start-session)
        (redirect
         (make-goto-path provider
                         (session)
                         (or redirect-uri *main*)))
      (redirect redirect-uri)))))

(restas:define-route receiver-route ("receiver/:provider/*states"
                                     :method :get)
;  (:sift-variables (provider #'parse-provider))
  (:additional-variables
   (state (hunchentoot:parameter "state"))
   (code (hunchentoot:parameter "code"))
   (error? (hunchentoot:parameter "error")))
(let ((provider (parse-provider provider)))
  (receive provider
           (or state (car states))  ;; It depends on provider, whether
                                    ;; it wants redirect URL to be
                                    ;; stable or uniq
           code error?)))

(restas:define-route logout-route ("logout" :method :get)
  (logout)
  (redirect *main*))