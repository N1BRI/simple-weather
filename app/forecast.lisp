;;;; forecast.lisp
;;;; Contains all code related to retrieving weather forecasts
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
   (detailed-forecast :initarg :detailed-forecast :accessor detailed-forecast)))

(defclass address-match()
  ((address :initarg :address :accessor address)
   (latitude :initarg :latitude :accessor latitude)
   (longitude :initarg :longitude :accessor longitude)))

(defun get-address-matches(street city state)
  "gets address matches for street, city, address from geocode api"
  (let ((resp (get-geocode-info street city state)))
    (when (status-ok resp)
      (parse-location-matches (first resp)))))
	
(defun get-geocode-info(street city state)
  "execute request to retrieve geocode info"
  (let ((request (build-geocode-request street city state)))
    (let ((response (multiple-value-bind(resp status)
			(drakma:http-request request :want-stream t)
		      (cons resp status))))
      (setf (first response)(st-json:read-json (first response)))
      (cons response "error parsing response")response)))

(defun parse-location-matches(geocode-info)
  "parse out address and lat,long from matches in geocode request response"
    (let ((address-matches(getjso* "result.addressMatches" geocode-info)))
      (when address-matches
	(loop for match in address-matches
	      collect(make-instance 'address-match 
				    :address (getjso "matchedAddress" match)
				    :latitude (getjso* "coordinates.y" match)
				    :longitude(getjso* "coordinates.x"  match)))))))

(defun build-geocode-request(street city state)
  "Build uri to hit the U.S. census bureau's geocode api"
  (quri:render-uri
   (quri:make-uri :defaults "https://geocoding.geo.census.gov/geocoder/locations/address"
		  :query (list(cons "street"  street)
			   (cons "city"  city)
			   (cons "state"  state)
			   (cons "benchmark"  2020)
			   (cons "format"  "json")))))

(defun get-forecast-links(latitude longitude)
  "get the links to forecasts provided by weather.gov for given geocode lat-long"
  (let ((response (multiple-value-bind (resp status)
		      (http-request (format nil "https://api.weather.gov/points/~a,~a" latitude longitude) :want-stream t)
		    (cons (st-json:read-json resp) status))))
    (handler-case ;we just want nil on failure
	(progn
	  (list 
	   (cons "seven-day" (getjso* "properties.forecast" (first response)))
	   (cons "hourly"  (getjso* "properties.forecastHourly" (first response)))))
      (t()))))


(defun get-forecast(url)
  (let ((response (multiple-value-bind(resp status)
		      (http-request url :want-stream t)
		    (cons (read-json resp) status))))response))

(defun status-ok(response)
  (ignore-errors (= (cdr response) 200)))


(defun map-json-to-forecasts(forecast-json)
  "maps json array of forecasts to corresponding clos obj"
  (loop for forecast in forecast-json
	collect(make-instance 'forecast
			      :start-time  (parse-timestring (getjso "startTime" forecast))
			      :end-time  (parse-timestring (getjso "endTime" forecast))
			      :is-day-time (getjso "isDaytime" forecast)
			      :name (getjso "name" forecast)
			      :icon (getjso "icon" forecast)
			      :temperature (getjso "temperature" forecast)
			      :wind-speed (getjso "windSpeed" forecast)
			      :wind-direction (getjso "windDirection" forecast)
			      :short-forecast (getjso "shortForecast" forecast)
			      :detailed-forecast (getjso "detailedForecast" forecast))))



(defun get-forecast-days(forecasts)
  "returns a list containing unique dates(month-day) from 7-ish day forecast"
  (remove-duplicates
   (loop for day in forecasts
	 collect (format-timestring nil (start-time day)
				    :format '(:month "/" :day "/" :year))) :test #'string=))

(defun get-forecast-days-list(forecasts)
  "coverts list of unique dates into list of lists with date as first element"
  (loop for day in (get-forecast-days forecasts)
	collect(list day)))
	  
(defun map-forecasts-to-dates(date-list forecasts)
    (loop for day in date-list
	  do(loop for forecast in forecasts
		  do(progn
		      (when (string= (car day)
				     (local-time:format-timestring
				      nil
				      (start-time forecast) :format '(:month "/" :day "/" :year)))
		        (push forecast (cdr day) ))))))

(defun map-forecasts-to-forecasts(date-list hours-list)
    (loop for day in date-list
	  do(loop for hour in hours-list
		  do(progn
		      (when (string= (car day)(car hour))
		        (push hour (cdr day) ))))))

(defun get-icon-identifiers(icon-url)
  "parses identifiers from icon url for forecast"
  (let* ((_(first(last (split "land" icon-url)))) ; break in half at 'land'	 
	 (__(first (split "?" _))))               ; split at question mark - take first half
    (remove "" (split "/" __) :test #'string=)))  ; split remains on '/' and remove empty strings
       

(defvar temp-list (list "skc"
"few"
"sct"
"bkn"
"ovc"
"wind_skc"
"wind_few"
"wind_sct"
"wind_bkn"
"wind_ovc"
"snow"
"rain_snow"
"rain_sleet"
"snow_sleet"
"fzra"
"rain_fzra"
"snow_fzra"
"sleet"
"rain"
"rain_showers"
"rain_showers_hi"
"tsra"
"tsra_sct"
"tsra_hi"
"tornado"
"hurricane"
"tropical_storm"
"dust"
"smoke"
"haze"
"hot"
"cold"
"blizzard"
"fog"))
