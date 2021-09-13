;;;; simple-weather.lisp
;;;; helpful api docs related to this code:
;;;; https://weather-gov.github.io/api/general-faqs
;;;; https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.html

(defpackage #:simple-weather
  (:use #:cl)
  (:import-from #:simple-weather.server
		#:start-server
		#:stop-server))
	

(in-package #:simple-weather)
