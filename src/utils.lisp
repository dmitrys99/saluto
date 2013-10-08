(in-package #:saluto)

(defun remove-zeros-from-string (array)
  "This function is needed by google.com provider,
because the answer of google.com for unknown reasons contains sudden chunks of zeros."
  (coerce (loop for x across array unless (zerop x)
               collect (code-char x))
          'string))

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

(defun md5 (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5
                             (sb-ext:string-to-octets str))))

(defun request (params)
  (let ((res (apply 'drakma:http-request params)))
    res))

(defun extract-access-token (provider-answer)
;;  (break "EAT: ~A" provider-answer)
  (let* ((pa (jsown:parse provider-answer))
         (res (jsown:val pa "access_token"))
         (user_id (json-val pa "user_id")))
    (if (and (stringp user_id) (string= user_id "")) 
        res
        ;; vk.com returns user_id field with access token.
        (list res user_id))))

(defun sort-params (params)
  (sort params 'string< :key 'car))

(defun concatenate-params (params &optional &key (delimiter "&"))
  (let* ((line (concatenate 'string "~{~A" delimiter "~}"))
         (r (format nil line
                    (loop for i in params
                       collect (concatenate 'string (car i) "=" (cdr i))))))
    (if delimiter
        (subseq r 0 (1- (length r)))
        r)))

(defun json-val (obj key)
  (handler-case
      (jsown:val obj key)
    (error (c)
      (declare (ignore c))
      "")))
