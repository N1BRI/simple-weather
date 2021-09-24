

(defpackage #:simple-weather.location
  (:use #:cl)
  (:use #:simple-weather.common)
  (:import-from #:st-json
		#:read-json
		#:getjso*)
  (:import-from #:quri
		#:render-uri
		#:make-uri)
  (:import-from #:drakma
		#:http-request)
  (:import-from #:spinneret
		#:with-html-string)
  (:import-from #:hunchentoot
		#:define-easy-handler))

(in-package #:simple-weather.location)


(hunchentoot:define-easy-handler (locate :uri "/locate" :default-request-type :post)(street city state)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((geocode-info (get-geocode-info street city state)))
    (when (= (cdr geocode-info) 200)
      (let ((location-matches (get-location-matches (car geocode-info))))
	(location-matches :stylesheets simple-weather.common:*styles* :matches location-matches)))))


(defun location-matches ( &key (stylesheets nil)(matches nil))
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:raw "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">")
      (when stylesheets
        (dolist (sheet stylesheets)
	  (:raw (concatenate 'string
			     "<link rel=\"stylesheet\" href=\"/app/static/css/" sheet "\"/>")))))
      (:body
       (:h2 "Simple U.S. Weather" )
       (:h5 "Just the U.S. weather")
       (:br)
       (:div
	(if matches
	    (progn
	      (:h4 (format nil "~a Matches Found" (length matches)))
	      (dolist (match matches)
		(:div
		 (:h5 (cdr (assoc "address" match :test #'string=)))
		 (:p (format nil "~a" (cdr (assoc "position" match :test #'string=))))
		 (:br)
		 (:a :href (format nil "/forecasts?lat=~a&long=~a"
				   (car (cdr (cdr (assoc "position" match :test #'string=))))
				   (first (cdr (assoc "position" match :test #'string=))))
		     :onclick (simple-weather.common:show-loader)
		     "get this forecast")
		 (:hr))))
	    (:h1 "There were no matches")))))))


