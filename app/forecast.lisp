;;;; forecast.lisp
;;;; Contains all code related to retrieving & parsing weather forecasts
;;;; helpful api docs related to this code:
;;;; https://weather-gov.github.io/api/general-faqs
;;;; https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.html

(in-package #:simple-weather.forecast)

(defclass forecast()
  ((start-time :initarg :start-time :accessor start-time)
   (end-time :initarg :end-time :accessor end-time)
   (is-day-time :initarg :is-day-time :accessor is-day-time)
   (name :initarg :name :accessor name)
   (icon :initarg :icon :accessor icon)
   (temperature :initarg :temperature :accessor temperature)
   (wind-speed :initarg :wind-speed :accessor wind-speed)
   (wind-direction :initarg :wind-direction :accessor wind-direction)
   (short-forecast :initarg :short-forecast :accessor short-forecast)
   (detailed-forecast :initarg :detailed-forecast :accessor detailed-forecast))
  (:documentation "12hr forecast from api.weather.gov"))

(defclass address-match()
  ((address :initarg :address :accessor address)
   (latitude :initarg :latitude :accessor latitude)
   (longitude :initarg :longitude :accessor longitude))
  (:documentation "address with latitude & longitude coords from geocoding.geo.census.gov"))

(defun get-address-forecast-url-list(street city state)
  "gets a list of address matches for the given address and corresponding weather.gov forecast links"
  (let ((address-matches (get-address-matches street city state)))
    (when address-matches
      (loop for match in address-matches
	    collect(list (address match)
			 (get-forecast-links (latitude match)
					     (longitude match)))))))
(defun get-forecast(url)
  "retrieves forecasts from api.weather.gov"
  (let ((response (get-request url)))
    (when (status-ok response)
      (let* ((forecast-json-array (getjso* "properties.periods" (first response)))
	     (forecasts (map-json-to-forecasts forecast-json-array)))
	(group-forecasts-by-start-date forecasts)))))

(defun get-address-matches(street city state)
  "[helper-function] gets address matches for street, city, address from geocode api"
  (let ((resp (get-geocode-info street city state)))
    (when (status-ok resp)
      (parse-location-matches (first resp)))))

(defun get-forecast-links(latitude longitude)
  "[helper-function] get the links to forecasts provided by weather.gov for given geocode lat-long"
  (let ((response (multiple-value-bind (resp status)
		      (http-request (format nil "https://api.weather.gov/points/~a,~a" latitude longitude) :want-stream t)
		    (cons (read-json resp) status))))
    (handler-case ;we just want nil on failure
	(progn
	  (list 
	   (cons "seven-day" (getjso* "properties.forecast" (first response)))
	   (cons "hourly"  (getjso* "properties.forecastHourly" (first response))))) ;; currently not using hourly
      (t()))))
	
(defun get-geocode-info(street city state)
  "[helper-function] execute request to retrieve geocode info"
  (let ((request (build-geocode-request street city state)))
    (let ((response (multiple-value-bind(resp status)
			(http-request request :want-stream t)
		      (cons resp status))))
      (setf (first response)(read-json (first response)))
      (cons response "error parsing response")response)))

(defun parse-location-matches(geocode-info)
  "[helper-function] parse out address and lat,long from matches in geocode request response"
    (let ((address-matches(getjso* "result.addressMatches" geocode-info)))
      (when address-matches
	(loop for match in address-matches
	      collect(make-instance 'address-match 
				    :address (getjso "matchedAddress" match)
				    :latitude (getjso* "coordinates.y" match)
				    :longitude(getjso* "coordinates.x"  match))))))
 
(defun build-geocode-request(street city state)
  "[helper-function] build uri to hit the U.S. census bureau's geocode api"
  (quri:render-uri
   (quri:make-uri :defaults "https://geocoding.geo.census.gov/geocoder/locations/address"
		  :query (list(cons "street"  street)
			   (cons "city"  city)
			   (cons "state"  state)
			   (cons "benchmark"  2020)
			   (cons "format"  "json")))))

(defun get-request(url)
  "[helper-function] simple wrapper for drakma-http-request"
  (let ((response (multiple-value-bind(resp status)
		      (http-request url :want-stream t)
		    (cons (read-json resp) status))))response))

(defun status-ok(response)
  "[helper-function] checks status of response"
  (ignore-errors (= (cdr response) 200)))


(defun map-json-to-forecasts(forecast-json)
  "[helper-function] maps json array of forecasts to corresponding clos obj"
  (loop for forecast in forecast-json
	collect(make-instance 'forecast
			      :start-time  (parse-timestring (getjso "startTime" forecast))
			      :end-time  (parse-timestring (getjso "endTime" forecast))
			      :is-day-time (getjso "isDaytime" forecast)
			      :name (getjso "name" forecast)
			      :icon (icon-url-to-icon (getjso "icon" forecast))
			      :temperature (getjso "temperature" forecast)
			      :wind-speed (getjso "windSpeed" forecast)
			      :wind-direction (getjso "windDirection" forecast)
			      :short-forecast (getjso "shortForecast" forecast)
			      :detailed-forecast (getjso "detailedForecast" forecast))))



	  
(defun group-forecasts-by-start-date(forecasts)
  "[helper-function] group forecasts by start date"
  (let ((date-list (get-forecast-days-list forecasts)))
    (setf date-list (get-forecast-days-list forecasts))
    (loop for day in date-list
	  do(loop for forecast in forecasts
		  do(progn
		      (when (string= (car day)
				     (local-time:format-timestring
				      nil
				      (start-time forecast) :format '(:month "/" :day "/" :year)))
		       (push forecast (cdr (last day)))))))date-list))


(defun get-forecast-days(forecasts)
  "[helper-function] returns a list containing unique dates(month/day/year) from 7-ish day forecast"
  (remove-duplicates
   (loop for day in forecasts
	 collect (format-timestring nil (start-time day)
				    :format '(:month "/" :day "/" :year))) :test #'string=))

(defun get-forecast-days-list(forecasts)
  "[helper-function] coverts list of unique dates into list of lists with date as first element"
  (loop for day in (get-forecast-days forecasts)
	collect(list day)))

(defun icon-url-to-icon (icon-url)
  "[helper-function] takes an icon-url and returns the matching weather icon classname"
 (let ((icon-ids (get-icon-identifiers icon-url)))
   (cond ((string= (first icon-ids) "day")
	  (first (rest (assoc (second icon-ids) *icon-map* :test #'string= ))))
	 ((string= (first icon-ids) "night")
	  (first(rest  (rest (assoc (second icon-ids) *icon-map* :test #'string=))))))))

(defun get-icon-identifiers(icon-url)
  "[helper-function] parses identifiers from icon-url"
  (let* ((_(first(last (split "land" icon-url))))
	 (__(first (split "?" _)))  ;split on ? take first
	 (___ (remove "" (split "/" __) :test #'string=)))
    (map 'list (lambda (x) (first (split "," x))) ___)))


(defparameter *icon-map* (list
		   '("skc". ("wi wi-day-sunny" "wi wi-night-clear"))
		   '("few". ("wi wi-day-cloudy-high" "wi wi-night-alt-cloudy-high" ))
		   '("sct". ("wi wi-day-cloudy" "wi wi-night-alt-partly-cloudy"))
		   '("bkn". ("wi wi-day-cloudy" "wi wi-night-alt-cloudy"))
		   '("ovc". ("wi wi-day-cloudy" "wi wi-night-alt-cloudy"))
		   '("wind_skc". ("wi wi-day-light-windy" "wi wi-night-alt-cloudy-windy"))
		   '("wind_few". ("wi wi-day-cloudy-windy" "wi wi-night-alt-cloudy-windy"))
		   '("wind_sct". ("wi wi-day-cloudy-windy" "wi wi-night-alt-cloudy-windy"))
		   '("wind_bkn". ("wi wi-day-cloudy-windy" "wi wi-night-alt-cloudy-windy"))
		   '("wind_ovc". ("wi wi-day-cloudy-windy" "wi wi-night-alt-cloudy-windy"))
		   '("snow". ("wi wi-day-snow" "wi wi-night-alt-snow"))
		   '("rain_snow".  ("wi wi-day-rain-mix" "wi wi-night-rain-mix"))
		   '("rain_sleet". ("wi wi-day-sleet" "wi wi-night-alt-sleet"))
		   '("snow_sleet". ("wi wi-day-sleet" "wi wi-night-alt-sleet"))
		   '("fzra". ("wi wi-day-sleet" "wi wi-night-alt-sleet"))
		   '("rain_fzra". ("wi wi-day-sleet" "wi wi-night-alt-sleet"))
		   '("snow_fzra". ("wi wi-day-sleet" "wi wi-night-alt-sleet"))
		   '("sleet". ("wi wi-day-sleet" "wi wi-night-alt-sleet"))
		   '("rain". ("wi wi-day-rain" "wi wi-night-alt-rain"))
		   '("rain_showers". ("wi wi-day-rain" "wi wi-night-alt-rain"))
		   '("rain_showers_hi". ("wi wi-day-rain" "wi wi-night-alt-rain"))
		   '("tsra". ("wi wi-day-thunderstorm" "wi wi-night-alt-thunderstorm"))
		   '("tsra_sct". ("wi wi-day-storm-showers" "wi wi-night-alt-storm-showers"))
		   '("tsra_hi". ("wi wi-day-storm-showers" "wi wi-night-alt-storm-showers"))
		   '("tornado". ("wi wi-tornado" "wi wi-tornado"))
		   '("hurricane". ("wi wi-hurricane" "wi wi-hurricane"))
		   '("tropical". ("storm" "wi wi-thunderstorm" "wi wi-thunderstorm"))
		   '("dust". ("wi wi-dust" "wi wi-dust"))
		   '("smoke". ("wi wi-smoke" "wi wi-smoke"))
		   '("haze". ("wi wi-day-haze" "wi wi-day-haze"))
		   '("hot". ("wi wi-hot" "wi wi-hot"))
		   '("cold". ("wi wi-snowflake-cold" "wi wi-snowflake-cold"))
		   '("blizzard". ("wi wi-snow" "wi wi-night-alt-snow"))
		   '("fog". ("wi wi-day-fog" "wi wi-night-fog"))))
