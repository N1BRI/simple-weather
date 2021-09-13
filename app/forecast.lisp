;;;; forecast.lisp
;;;; Contains all code related to retrieving weather forecasts
;;;; helpful api docs related to this code:
;;;; https://weather-gov.github.io/api/general-faqs
;;;; https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.html

(defpackage #:simple-weather.forecast
  (:use #:cl)
  (:import-from #:st-json
		#:read-json
		#:getjso*)
  (:import-from #:quri
		#:render-uri
		#:make-uri)
  (:import-from #:drakma
		#:http-request))

(in-package #:simple-weather.forecast)


(defun get-geocode-info(street city state)
  "Execute request to retrieve geocode info"
  (let ((request (build-geocode-request street city state)))
    (let ((response (multiple-value-bind(resp status)
			(drakma:http-request request :want-stream t)
		      (cons resp status))))
      (setf (first response)(st-json:read-json (first response)))
      (cons response "error parsing response")response)))

(defun get-latitude-longitude(geocode-info)
  "Parse the latitude and longitude from geocode info -- returns (lat . long) or nil"
    (let ((first-address-match (first (st-json:getjso* "result.addressMatches" geocode-info))))
      (when first-address-match
	(cons (st-json:getjso* "coordinates.y" first-address-match)
	      (st-json:getjso* "coordinates.x" first-address-match)))))

(defun build-geocode-request(street city state)
  "Build uri to hit the U.S. census bureau's geocode api"
  (quri:render-uri
   (quri:make-uri :defaults "https://geocoding.geo.census.gov/geocoder/locations/address"
		  :query (list(cons "street"  street)
			   (cons "city"  city)
			   (cons "state"  state)
			   (cons "benchmark"  2020)
			   (cons "format"  "json")))))

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

(defun get-hourly-forecast(url)
  "retrieves hourly forecast for the next 7 days starting from time of request"
  (st-json:read-json (drakma:http-request hourly-url :want-stream t)))

  (defun status-ok(response)
    (ignore-errors (= (cdr response) 200)))
