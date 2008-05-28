
(defpackage :slimtimer 
  (:use :cl)
  (:export :with-xml :start-session :list-tasks :list-entries))
(in-package :slimtimer)

(require 'drakma)
(require 's-xml)
(require 'babel)
(require 'cl-who)

(setq cl-who:*attribute-quote-char* #\")

(defparameter *session-token* nil)
(defparameter *session-uid* nil)
(defparameter *api-key* nil)

(defun start-session (username password api-key)
  (let ((resp-body nil)
 	(resp-status nil)
 	(resp-headers nil)
	(resp-uri nil)
	(resp-stream nil)
	(resp-must-close nil)
	(resp-reason nil))
    (multiple-value-setq (resp-body resp-status resp-headers resp-uri
				    resp-stream resp-must-close resp-reason)
      (drakma:http-request "http://www.slimtimer.com/users/token"
			   :method :post
			   :content-type "application/xml"
			   :accept "application/xml"
			   :content (with-xml
				      (:request 
				       (:user
					(:email (cl-who:str username))
					(:password (cl-who:str password)))
				       (:api-key (cl-who:str api-key))))))
    (if (eql resp-status 200)
	(destructuring-bind (r (ac token) ((ui ty i) id))
	    (s-xml:parse-xml-string (babel:octets-to-string resp-body))
	  (declare (ignore r ac ui ty i))
	  (setq *api-key* api-key)
	  (setq *session-uid* id)
	  (setq *session-token* token))
	(error "error response from server: ~a~%~s" 
	       resp-status (babel:octets-to-string resp-body)))))

(defun list-tasks ()
  (let ((resp-body nil)
 	(resp-status nil)
 	(resp-headers nil)
	(resp-uri nil)
	(resp-stream nil)
	(resp-must-close nil)
	(resp-reason nil))
    (multiple-value-setq (resp-body resp-status resp-headers resp-uri
				    resp-stream resp-must-close resp-reason)
      (drakma:http-request (mkstr "http://www.slimtimer.com/users/" *session-uid* "/tasks?"
				  "api_key=" *api-key* "&access_token=" *session-token*)
			   :method :get
			   :accept "application/xml"))
    (s-xml:parse-xml-string (babel:octets-to-string resp-body))))

(defun list-entries (range &optional filter)
  (let* ((resp-body nil)
	 (resp-status nil)
	 (resp-headers nil)
	 (resp-uri nil)
	 (resp-stream nil)
	 (resp-must-close nil)
	 (resp-reason nil)
	 (entry-list nil)
	 (url (mkstr "http://www.slimtimer.com/users/" *session-uid* "/time_entries?"
		     "api_key=" *api-key* "&access_token=" *session-token*)))
    (when range
      (setq url (mkstr url "&range_start=" (car range) "&range_end=" (cadr range))))
    (format t "doing get: ~%~s" url)
    (multiple-value-setq (resp-body resp-status resp-headers resp-uri
				    resp-stream resp-must-close resp-reason)
      (drakma:http-request url :method :get
			   :accept "application/xml"))
    (if (eql resp-status 200)
	(progn
	  (setq entry-list (s-xml:parse-xml-string (babel:octets-to-string resp-body)))
	  (if filter
	      (mapcan filter entry-list)
	      entry-list))
	(error "error response from server: ~a~%~s"
	       resp-status (babel:octets-to-string resp-body)))))
	
	

					    
						    