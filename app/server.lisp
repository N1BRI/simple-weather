;;;; server.lisp -- contains principal server logic

(in-package :simple-weather.server)

(defparameter *app-acceptor* nil)
(defparameter root (asdf:system-relative-pathname "simple-weather" "app"))
(setf hunchentoot::*show-lisp-errors-p* t)
(defparameter *app*
  (make-instance 'hunchentoot:easy-acceptor
                 :document-root root
                 :error-template-directory (merge-pathnames "app/static/error-templates/" root)
                ; :access-log-destination (merge-pathnames "logs/access.log" root)
                ; :message-log-destination (merge-pathnames "logs/message.log" root)
                 :port 8111))

(defun start-server()
  (hunchentoot:start *app*))
(defun stop-server()
  (hunchentoot:stop *app*))

