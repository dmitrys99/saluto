(in-package #:saluto)

(defun session ()
  (when hunchentoot:*session*
    (slot-value hunchentoot:*session* 'hunchentoot::session-string)))

(defun redirect (path)
;  (break "~A" path)
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

(defun md5 (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5
                             (sb-ext:string-to-octets str))))

(defun request (params)
  (apply 'drakma:http-request params))

(defun extract-authorization-key (provider-answer)
  (let ((res (jsown:val (jsown:parse provider-answer) "access_token")))
    res))

(defun sort-params (params)
  (sort params 'string< :key 'car))

(defun concatenate-params (params &optional &key (delimiter "&"))
  (let* ((line (concatenate 'string "~{~A" delimiter "~}"))
         (r (format nil line
                   (loop for i in params
                      collect (concatenate 'string (car i) "=" (cdr i))))))
    (if delimiter
    (subseq r 0 (1- (length r)))
    r)
    ))
