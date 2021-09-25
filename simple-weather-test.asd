
(asdf:defsystem #:simple-weather-test
  :author "brian beegan <brianbeegan@protonmail.com>"
  :license ""
  :depends-on (:simple-weather
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "forecast")))))
