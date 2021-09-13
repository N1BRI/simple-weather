;;;; simple-weather.asd

(asdf:defsystem #:simple-weather
  :description "Simple-weather is just a simple interface for weather.gov's local forecast apis"
  :author "Brian Beegan brianbeegan@protonmail.com"
  :license  "Public Domain"
  :version "0.0.1"
  :serial t
  :depends-on (#:drakma
	       #:hunchentoot
	       #:cl-css
	       #:spinneret
	       #:st-json
	       #:quri)
  :components ((:module "app"
		:serial t
		:components ((:file "forecast")
			     (:file "server")
			     (:static-file "css/marx.min.css")))
	       (:file "simple-weather")))
