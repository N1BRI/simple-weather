

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


(defun get-geocode-info(street city state)
  "Execute request to retrieve geocode info"
  (let ((request (build-geocode-request street city state)))
    (let ((response (multiple-value-bind(resp status)
			(drakma:http-request request :want-stream t)
		      (cons resp status))))
      (setf (first response)(st-json:read-json (first response)))
      (cons response "error parsing response")response)))

(defun get-location-matches(geocode-info)
  "parse out address and lat,long from matches in geocode request response"
    (let ((address-matches(st-json:getjso* "result.addressMatches" geocode-info)))
      (when address-matches
	(loop for match in address-matches
	      collect(list 
		      (cons "address" (st-json:getjso* "matchedAddress" match))
		      (cons "position" (list
					(st-json:getjso* "coordinates.x" match)
					(st-json:getjso* "coordinates.y"  match))))))))

(defun build-geocode-request(street city state)
  "Build uri to hit the U.S. census bureau's geocode api"
  (quri:render-uri
   (quri:make-uri :defaults "https://geocoding.geo.census.gov/geocoder/locations/address"
		  :query (list(cons "street"  street)
			   (cons "city"  city)
			   (cons "state"  state)
			   (cons "benchmark"  2020)
			   (cons "format"  "json")))))
