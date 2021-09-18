;;;; forecast.lisp
;;;; Contains all code related to retrieving weather forecasts
;;;; helpful api docs related to this code:
;;;; https://weather-gov.github.io/api/general-faqs
;;;; https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.html

(defpackage #:simple-weather.forecast
  (:use #:cl)
  (:use #:simple-weather.common)
  (:import-from #:st-json
		#:read-json
		#:getjso*)
  (:import-from #:drakma
		#:http-request))

(in-package #:simple-weather.forecast)

(hunchentoot:define-easy-handler (forecasts-handler :uri "/forecasts")(lat long)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((forecasts (get-weather-gov-forecast-links lat long)))
    (forecasts-page :stylesheets simple-weather.common:*styles* :matches forecasts))) 

(defun forecasts-page(&key (stylesheets nil)(matches nil))
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:raw "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">")
      (when stylesheets
        (dolist (sheet stylesheets)
	  (:raw (concatenate 'string
			     "<link rel=\"stylesheet\" href=\"/app/static/css/" sheet "\"/>")))))
      (:body (:p (format nil "~a" matches))))))

(defun get-weather-gov-forecast-links(latitude longitude)
  "get the links to forecasts provided by weather.gov for given geocode lat-long"
  (let ((response (multiple-value-bind (resp status)
		      (drakma:http-request (format nil "https://api.weather.gov/points/~a,~a" latitude longitude) :want-stream t)
		    (cons (st-json:read-json resp) status))))
    (handler-case ;we just want nil on failure
	(progn
	  (list (cons "hourly"  (st-json:getjso* "properties.forecastHourly" (first response)))
		(cons "seven-day" (st-json:getjso* "properties.forecast" (first response)))))
      (t()))))


(defun get-forecast(url)
  (let ((response (multiple-value-bind(resp status)
		      (drakma:http-request url :want-stream t)
		    (cons (st-json:read-json resp) status))))response))

  (defun status-ok(response)
    (ignore-errors (= (cdr response) 200)))


;; (defun get-all-forecasts(street city state)
;;   (let ((gci (get-geocode-info street city state)))
;;      (when (status-ok gci)
;;        (let ((lat-long (get-latitude-longitude (first gci))))
;;  	 (when lat-long
;;  	  (let ((urls (get-weather-gov-forecast-links (car lat-long)(cdr lat-long))))
;; 	    (let ((seven-day (get-forecast (cdr (assoc "seven-day" urls :test #'string=))))
;; 		  (hourly (get-forecast (cdr (assoc "hourly" urls :test #'string=)))))
;; 	      (list seven-day hourly))))))))
