(defpackage #:simple-weather-test
  (:use :cl
   :fiveam
   :simple-weather))
(in-package :simple-weather-test)

;;; root suite
(5am:def-suite test-simple-weather
  :description "test simple-weather")

(5am:def-suite forecast-tests
  :description "tests simple-weather.forecast"
  :in test-simple-weather)


(5am:in-suite forecast-tests)

(5am:test get-address-forecast-url-list-valid-address
  (let ((response (simple-weather.forecast:get-address-forecast-url-list "10 first street" "new york" "new york")))
    (is (= (length response) 3))))

(5am:test get-address-forecast-url-list-empty-state
  (let ((response (simple-weather.forecast:get-address-forecast-url-list "10 first street" "new york" "")))
    (is (null response))))

(5am:test get-address-forecast-url-list-junk-values
  (let ((response (simple-weather.forecast:get-address-forecast-url-list
		   "323" "&*&*78" "dsadsd")))
    (is (null response))))

(5am:test get-forecast-valid-7-day
  (let ((forecasts (simple-weather.forecast:get-forecast "https://api.weather.gov/gridpoints/OKX/32,34/forecast")))
    (is (>= (length forecasts) 7))))

(5am:test get-forecast-junk-url
  (let ((forecasts (simple-weather.forecast:get-forecast "https://api.weather.gov/gridpoints/OKX/32,34/foreca")))
    (is (null forecasts))))
