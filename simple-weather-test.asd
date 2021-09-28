
(asdf:defsystem #:simple-weather-test
  :author "brian beegan <brianbeegan@protonmail.com>"
  :license ""
  :depends-on (:simple-weather
               :parachute)
  :components ((:module "t"
                :serial t
                :components
                ((:file "simple-weather")))))
