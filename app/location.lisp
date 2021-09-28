;;;; location.lisp -- view for location matches

(in-package #:simple-weather.location)


(define-easy-handler (locate :uri "/locate" :default-request-type :post)
    (street city state)
  (setf (hunchentoot:content-type*) "text/html")
  (let ((matches (get-address-forecast-url-list street city state)))
    (with-page (:title "Simple Weather")(:styles *styles*)
      (:div :class "banner" (:a :href "/" (:h2 "Simple U.S. Weather" ))
      (:h5 "Just the U.S. weather"))
      (:br)
      (:div
       (cond ((not (null matches))
	      (if (= (length matches) 1)
		  (:h4 (format nil "~a Match Found" (length matches)))
		  (:h4 (format nil "~a Matches Found" (length matches))))
	      (:table
	       (dolist (match matches)
		 (:form :action "/forecast" :method "POST"
			(:tr (:td (:label (first match)))
			     (:input :type "hidden"
				     :name "link"
				     :value (cdr (assoc "seven-day" (second match):test #'string=)))
			     (:input :type "hidden"
				     :name "address"
				     :value (format nil "~a ~a ~a" street city state))
			     (:td (:button "View Forecast")))))))
	     ((null matches)
	      (:div :class "no-match"
	       (:h4 "No matches found ☹")
	       (:a :href "/" "Run another search?"))))
	     ))))

