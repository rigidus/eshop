

(in-package #:eshop.storage)


;; ;; main

;; (restas:define-route main ("/")
;;   "manifest stub")

;; (restas:define-route resto-list ("/restaurants")
;;   (let ((rs)
;;         (lang (aif (hunchentoot:get-parameter "lang") it "ru")))
;;     (maphash #'(lambda (k v)
;;                  (push (make-instance
;;                         'resto~shortlist
;;                         :id (id v)
;;                         :name (funcall (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage) (name v))
;;                         :price (price v)
;;                         :photo (photo v)
;;                         :address (let ((w (address v)))
;;                                    (make-instance
;;                                     'address
;;                                     :latitude (latitude w)
;;                                     :longitude (longitude w)
;;                                     :postal_code (postal_code w)
;;                                     :country (funcall
;;                                               (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
;;                                               (gethash (country w) *h-country*))
;;                                     :city (funcall
;;                                            (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
;;                                            (gethash (city w) *h-city*))
;;                                     :subway (funcall
;;                                              (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
;;                                              (subway w))
;;                                     :street (funcall
;;                                              (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
;;                                              (street w))
;;                                     :building (building w)))
;;                         :estimate (estimate v))
;;                        rs))
;;              *h-resto*)
;;     (format nil "{\"response\": ~A}" (json:encode-json-to-string (reverse rs)))))


;; (restas:define-route resto-id ("/restaurant/:id/about")
;;   (let ((rs)
;;         (lang (aif (hunchentoot:get-parameter "lang") it "ru"))
;;         (v (gethash (parse-integer id :junk-allowed t) *h-resto*)))
;;     (setf rs (make-instance
;;               'resto~longview
;;               :id (id v)
;;               :name (funcall (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage) (name v))
;;               :descr (funcall (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage) (descr v))
;;               :price (price v)
;;               :photo (photo v)
;;               :site (site v)
;;               :phone (phone v)
;;               :address (let ((w (address v)))
;;                          (make-instance
;;                           'address
;;                           :latitude (latitude w)
;;                           :longitude (longitude w)
;;                           :postal_code (postal_code w)
;;                           :country (funcall
;;                                     (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
;;                                     (gethash (country w) *h-country*))
;;                           :city (funcall
;;                                  (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
;;                                  (gethash (city w) *h-city*))
;;                           :subway (funcall
;;                                    (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
;;                                    (subway w))
;;                           :street (funcall
;;                                    (intern (string-upcase (format nil "i18n-str-~A" lang)) :eshop.storage)
;;                                    (street w))
;;                           :building (building w)))
;;               :estimate (estimate v)
;;               :capacity (capacity v)
;;               :optional (optional v)
;;               ))
;;     (format nil "{\"response\": ~A}" (json:encode-json-to-string rs))))



(restas:define-route resto-id ("/restaurant/:id/menu")
  (let ((rs)
        (lang (aif (hunchentoot:get-parameter "lang") it "ru"))
        (v (gethash (parse-integer id :junk-allowed t) *h-resto*))
        (categoryes)
        (products))
    ;; (setf rs (make-instance
    ;;           'category
    (format nil "
[
    {
        \"categories\" :
        {
            \"id\":\"1\",
            \"name\" : \"Акции\",
            \"icon\":\"/img/icon_action.png\"
        },
        {
            \"id\":\"2\",
            \"name\" : \"Специальные предложения\",
            \"icon\":\"/img/icon_spec.png\"
        }
    },
    {
        \"products\" :
        {
            \"id\":\"1\",
            \"name\" : \"Еда мужская, 1 кг.\",
            \"pic\":\"/img/eda.png\"
        }
    }
]")))

