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
  ;;  (break "~A" path)
  (info-message (format nil "REDIRECTED: ~A" path))
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
  (info-message (format nil "REQUEST: ~A" params))
  (let ((res (apply 'drakma:http-request params)))
    (info-message (format nil "REQUEST RES: ~A" res))
    res))

(defun extract-access-token (provider-answer)
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
        r)))

(defvar *logger* nil)

(defun init-logger ()

  (setf *logger* (log:category :saluto))

  (log:config *logger*
	      :immediate-flush t
	      :sane
	      :daily   (pathname (concatenate 'string *saluto-log-prefix* "/" *saluto-log*))
	      :pattern "SLT: %D{%Y-%m-%d %H:%M:%S} [%p] %t %m%n")

  (info-message "Saluto logging started"))

(defun untilde (s)
  (if (stringp s)
      (multiple-value-bind (a b)
	  (cl-ppcre:regex-replace-all "~" s "~1~")
	(declare (ignore b))a)
      nil))

(defmacro messages-defun (&rest message-types)
  (cons 'progn
        (loop
           for message-type in message-types
           for fn-name = (intern (format nil "~a-MESSAGE" message-type) :saluto)
           for log-fn  = (intern (string message-type) :log)
           collecting
             `(defun ,fn-name (message)
                (,log-fn :logger *logger* (untilde message))))))

(messages-defun :debug :error :info :warning)
