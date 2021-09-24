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
			  #:split))
