(in-package #:saluto)

(defvar *providers* '()
  "Variable, containing providers objects
Should be replaced when mounting module")

(defvar *logged-in-p-fun*
  (lambda ()
    (error "LOGGED-IN-P: Not implemented"))
  "Answers the question whether current user is really logged in.
To be replaced when mounting module.
Is needed only for LOGIN-WITH route.  Can be safely set to (CONSTANTLY NIL).")

(defun parse-provider (provider-name)
  (or (find provider-name *providers* :key #'name :test #'string=)
      (error "SALUTO: No such provider ~A" provider-name)))

(restas:define-route login-with ("goto/:provider/" :method :get)
  (:sift-variables (provider #'parse-provider))
  (:additional-variables (redirect-uri (hunchentoot:parameter "redirect")))
  ;; This REDIRECT-URI means just target page after successful login
  (if (or (not (session))
          (not (funcall *logged-in-p-fun*)))
      (progn
        (start-session)
        (redirect
         (make-goto-path provider
                         (session)
                         (or redirect-uri *main*))))
      (redirect (or redirect-uri *main*))))

(defun make-states-back (states)
  (when states
    (format nil "~{~a~^/~}" states)))

(restas:define-route receiver-route ("receiver/:provider/*states"
                                     :method :get)
  (:sift-variables (provider #'parse-provider))
  (:additional-variables
   (state (hunchentoot:parameter "state"))
   (code (hunchentoot:parameter "code"))
   (error? (hunchentoot:parameter "error")))
  (receive provider
           (or state (make-states-back states))  ;; It depends on
                                                 ;; provider, whether
                                                 ;; it wants redirect
                                                 ;; URL to be stable
                                                 ;; or uniq
           code error?))

(restas:define-route logout-route ("logout" :method :get)
  (logout)
  (redirect *main*))