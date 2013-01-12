(in-package #:saluto)

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
