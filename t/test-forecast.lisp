(defpackage #:simple-weather-test.forecast
  (:use :cl
   :parachute
	:simple-weather.forecast)
  (:export #:run-forecast-tests))
(in-package :simple-weather-test.forecast)


(define-test get-address-forecast-url-list-tests)

(define-test with-valid-address
  :parent  get-address-forecast-url-list-tests
  (is = 3 (length (get-address-forecast-url-list
		   "10 first street" "new york" "ny"))))

(define-test with-missing-city
  :parent  get-address-forecast-url-list-tests
  (null (length (get-address-forecast-url-list
		 "10 first street" " " "ny"))))

(define-test with-missing-junk
  :parent  get-address-forecast-url-list-tests
  (null (length (get-address-forecast-url-list
		 "1dsad2325340 first street" "990322!!@$" "MN"))))


(define-test get-forecast-tests)

(define-test should-return-forecasts
  :parent get-forecast-tests
  (let ((response (get-forecast "https://api.weather.gov/gridpoints/OKX/32,34/forecast")))
    (of-type 'forecast (second (first response)))))
  
(defun run-forecast-tests()
  (test 'get-address-forecast-url-list-tests)
  (test 'get-forecast-tests))
