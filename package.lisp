(defpackage #:simple-weather.forecast
  (:use #:cl)
  (:shadowing-import-from #:st-json
			  #:read-json
			  #:getjso*
			  #:getjso)
  (:shadowing-import-from #:drakma
			  #:http-request)
  (:shadowing-import-from #:hunchentoot
			  #:define-easy-handler)
  (:shadowing-import-from #:spinneret
			  #:with-html-string)
  (:shadowing-import-from #:local-time
			  #:format-timestring
			  #:parse-timestring)
  (:shadowing-import-from #:str
			  #:split)
  (:export #:forecast
	   #:address-match
	   #:get-address-forecast-url-list
	   #:get-forecast))



(defpackage #:simple-weather.common
  (:use #:cl)
  (:import-from #:spinneret
		#:with-html-string
		#:with-html)
  (:import-from #:parenscript
		#:ps
		#:getprop)
  (:export *styles*
	   #:show-loader
	   #:with-page))


(defpackage #:simple-weather.landing
  (:use #:cl)
  (:use #:simple-weather.common)
  (:import-from #:hunchentoot
		#:define-easy-handler)
  (:import-from #:spinneret
		#:with-html-string))

(defpackage #:simple-weather.server
  (:use #:cl)
  (:import-from #:hunchentoot
		#:start
		#:stop
		#:define-easy-handler
		#:started-p)
  (:export #:start-server
	   #:stop-server))

(defpackage #:simple-weather.weather
  (:use #:cl)
  (:use #:simple-weather.common)
  (:shadowing-import-from #:spinneret
		#:with-html-string)
  (:shadowing-import-from #:hunchentoot
			  #:define-easy-handler)
  (:shadowing-import-from #:simple-weather.forecast
			  #:get-forecast
			  #:forecast)
  (:shadowing-import-from #:local-time
			  #:format-timestring))

(defpackage #:simple-weather.location
  (:use #:cl)
  (:use #:simple-weather.common)
  (:shadowing-import-from #:spinneret
		#:with-html-string)
  (:shadowing-import-from #:hunchentoot
			  #:define-easy-handler)
  (:import-from #:simple-weather.forecast
		#:get-address-forecast-url-list))

(defpackage #:simple-weather
  (:use #:cl)
  (:import-from #:simple-weather.server
		#:start-server)
  (:export #:run-app))

