build:
	sbcl --load simple-weather.asd \
	     --eval '(ql:quickload :simple-weather)' \
         --eval "(sb-ext:save-lisp-and-die #p\"simple-weather\" :toplevel #'simple-weather:run-app :executable t)"

