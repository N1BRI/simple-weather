;;;; simple-weather.lisp
;;;; top-level entry point

(in-package #:simple-weather)

(defun run-app()
  (simple-weather.server:start-server)
   (handler-case (bt:join-thread (find-if (lambda (th)
                                            (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      () (progn
           (format *error-output* "Aborting.~&")
           (simple-weather.server:stop-server)
           (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))
