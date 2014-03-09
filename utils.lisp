(in-package #:saluto)

(defun make-state (session redirect-uri)
  (concatenate 'string session "X" redirect-uri))

(defun split-state (state)
  (if (and (stringp state)
           (find #\X state :test #'char=))
      (let ((split (position #\X state :test #'char=)))
        (list (subseq state 0 split)      ;; Session string
              (subseq state (1+ split)))) ;; Redirect uri
      (list nil nil)))

(defun list-to-link (base-url &rest params)
  (format nil "~a?~{~a=~a~^&~}" base-url params))

(defun session ()
  (when hunchentoot:*session*
    (slot-value hunchentoot:*session* 'hunchentoot::session-string)))

(defun redirect (path)
  (hunchentoot:redirect path))

(defun start-session ()
  (hunchentoot:start-session))

(defun logout ()
  (if (session)
      (progn
        (hunchentoot:remove-session hunchentoot:*session*)
        (setf hunchentoot:*session* nil)
        t)
      nil))

(defun invalid-receiver-params? (code session error?)
  (or error?
      (null code)
      (null session)
      (null (session))
      (and (string/= (session) session))))

(defun request (params)
#+SALUTO-DEBUG  (break "request: params: ~A" params)
  (let ((res 
      (apply 'drakma:http-request params)))
#+SALUTO-DEBUG    (break "request: res: ~A uncoded ~A" res (sb-ext:octets-to-string res))
    res))

(defun concatenate-params (params)
  (with-output-to-string (out)
    (format out "~a=~a" (caar params) (cdar params))
    (loop for (name . value) in (cdr params) do
         (format out "&~a=~a" name value))))

(defun json-val (obj key)
  (handler-case
      (jsown:val obj key)
    (error (c)
      (declare (ignore c))
      "")))
