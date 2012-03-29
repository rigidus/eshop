

(in-package #:eshop.storage)


;; main

(restas:define-route main ("/")
  "manifest stub")

(restas:define-route rest-list ("/restaurants")
  (let ((rs)
        (lang (aif (hunchentoot:get-parameter "lang") it "ru")))
    (maphash #'(lambda (k v)
                 (push (make-instance
                        'restaurant~shortlist
                        :id (id v)
                        :name (funcall (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage) (name v))
                        :price (price v)
                        :photo (photo v)
                        :address (let ((w (address v)))
                                   (make-instance
                                    'address
                                    :latitude (latitude w)
                                    :longitude (longitude w)
                                    :postal_code (postal_code w)
                                    :country (funcall
                                              (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
                                              (gethash (country w) *h-country*))
                                    :city (funcall
                                           (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
                                           (gethash (city w) *h-city*))
                                    :subway (funcall
                                             (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
                                             (subway w))
                                    :street (funcall
                                             (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
                                             (street w))
                                    :building (building w)))
                        :estimate (estimate v))
                       rs))
             *h-restaurant*)
    (format nil "<pre>{\"response\": ~A}</pre>" (json:encode-json-to-string (reverse rs)))))


(restas:define-route rest-id ("/restaurant/:id")
  (let ((rs)
        (lang (aif (hunchentoot:get-parameter "lang") it "ru"))
        (v (gethash (parse-integer id :junk-allowed t) *h-restaurant*)))
    (setf rs (make-instance
              'restaurant~longview
              :id (id v)
              :name (funcall (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage) (name v))
              :descr (funcall (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage) (descr v))
              :price (price v)
              :photo (photo v)
              :site (site v)
              :phone (phone v)
              :address (let ((w (address v)))
                         (make-instance
                          'address
                          :latitude (latitude w)
                          :longitude (longitude w)
                          :postal_code (postal_code w)
                          :country (funcall
                                    (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
                                    (gethash (country w) *h-country*))
                          :city (funcall
                                 (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
                                 (gethash (city w) *h-city*))
                          :subway (funcall
                                   (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
                                   (subway w))
                          :street (funcall
                                   (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
                                   (street w))
                          :building (building w)))
              :estimate (estimate v)
              :capacity (capacity v)
              :optional (optional v)
              ))
    (format nil "<pre>{\"response\": ~A}</pre>" (json:encode-json-to-string rs))))
