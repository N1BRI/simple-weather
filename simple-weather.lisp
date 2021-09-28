;;;; simple-weather.lisp
;;;; top-level entry point

(in-package #:simple-weather)

(defun run-app()
  (simple-weather.server:start-server)
  (sb-thread:join-thread (find-if
                          (lambda (th)
                            (string= (sb-thread:thread-name th) "hunchentoot-listener-1"))
                          (sb-thread:list-all-threads))))
