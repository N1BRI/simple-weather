;;;; simple-weather.lisp
;;;; helpful api docs related to this code:
;;;; https://weather-gov.github.io/api/general-faqs
;;;; https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.html

(defpackage #:simple-weather
  (:use #:cl)
  (:import-from #:simple-weather.server
		#:start-server)
  (:export #:run-app))
	

(in-package #:simple-weather)

(defun run-app()
  (simple-weather.server:start-server)
  (sb-thread:join-thread (find-if
                          (lambda (th)
                            (string= (sb-thread:thread-name th) "hunchentoot-listener-1"))
                          (sb-thread:list-all-threads))))
