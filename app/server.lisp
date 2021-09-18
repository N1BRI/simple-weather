
(defpackage #:simple-weather.server
  (:use #:cl)
  (:import-from #:hunchentoot
		#:start
		#:stop
		#:define-easy-handler
		#:started-p)
  (:export #:start-server
	   #:stop-server))

(in-package :simple-weather.server)

(defparameter *app-acceptor* nil)

(defparameter *styles* (list "marx.min.css" "base.css"))

(defun start-server (&optional (port 4242) (document-root ""))
  "entry point -- starts up hunchentoot acceptor if not already running"
  (setf *app-acceptor* (make-instance 'hunchentoot:easy-acceptor :port port
								:document-root document-root))
  (unless (hunchentoot:started-p *app-acceptor*)
    (hunchentoot:start *app-acceptor*)))

(defun stop-server()
  (hunchentoot:stop *app-acceptor*))
