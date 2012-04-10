;;;; routes.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(in-package #:eshop.storage)


;; main

(restas:define-route main ("/")
  (format nil "~{~A<br/>~}"
          (list
           "/lang? [lang=ru]"
           "/country? [lang=ru]"
           "/city? [country=rus] & [lang=ru]"
           "/subway? [city=spb] & [lang=ru]"
           )))


;; GET-LANGUAGES
(defun get-languages (&key (lang "ru"))
  (loop :for item :in (with-connection *db-spec* (select-dao 'lang)) :collect
     (list
      (cons 'code (code item))
      (cons 'name (car (mapcar #'name (load-options item :lang lang))))
      )))

;; (json:encode-json-to-string (get-languages :lang "en"))

(restas:define-route lang ("/lang")
  "/lang? [lang=ru]"
  (format nil "{\"response\": ~A}" (json:encode-json-to-string (get-languages :lang (aif (hunchentoot:get-parameter "lang") it "ru")))))


;; GET-COUNTYES
(defun get-countryes (&key (lang "ru"))
  (loop :for item :in (with-connection *db-spec* (select-dao 'country)) :collect
     (list (cons 'id (id item))
           (cons 'code (code item))
           (cons 'name (car (mapcar #'val (remove-if-not #'(lambda (x)
                                                             (equal (lang-id x) (get-lang-id lang)))
                                                         (load-values (car (load-options item :name "name"))))))))))

;; (json:encode-json-to-string (get-countryes))

(restas:define-route country ("/country")
  "/country? [lang=ru]"
  (format nil "{\"response\": ~A}" (json:encode-json-to-string (get-countryes :lang (aif (hunchentoot:get-parameter "lang") it "ru")))))


;; GET-CITYES
(defun get-cityes (&key (country "rus") (lang "ru"))
  (let ((country-id (if (typep country 'integer)
                        country
                        (let ((dao-obj-lst (select-dao 'country (:= 'code country))))
                          (if (null dao-obj-lst)
                              (return-from get-cityes "country not found")
                              (id (car dao-obj-lst)))))))
    (loop :for item :in (with-connection *db-spec* (select-dao 'city (:= 'country-id country-id))) :collect
       (list
        (cons 'id   (id item))
        (cons 'code (code item))
        (cons 'name (car (mapcar #'val (remove-if-not #'(lambda (x)
                                                          (equal (lang-id x) (get-lang-id lang)))
                                                      (load-values (car (load-options item :name "name")))))))))))

;; (get-cityes :country "usa" :lang "en")

(restas:define-route city ("/city")
  "/city? [country=rus] & [lang=ru]"
  (format nil "{\"response\": ~A}"
          (json:encode-json-to-string
           (get-cityes :country (aif (hunchentoot:get-parameter "country") it "rus")
                       :lang (aif (hunchentoot:get-parameter "lang") it "ru")))))

;; (get-cityes "usa")

(defun get-subways (&key (city "spb") (lang "ru"))
  (let ((city-id (if (typep city 'integer)
                     city
                     (let ((dao-obj-lst (select-dao 'city (:= 'code city))))
                       (if (null dao-obj-lst)
                           (return-from get-subways "city not found")
                           (id (car dao-obj-lst)))))))
    (loop :for item :in (with-connection *db-spec* (select-dao 'subway (:= 'city-id city-id))) :collect
       (list
        (cons 'id   (id item))
        (cons 'code (code item))
        (cons 'name (car (mapcar #'val (remove-if-not #'(lambda (x)
                                                          (equal (lang-id x) (get-lang-id lang)))
                                                      (load-values (car (load-options item :name "name")))))))))))

;; (json:encode-json-to-string (get-subways :city "spbs" :lang "en"))

(restas:define-route subway ("/subway")
  "/subway? [city=spb] & [lang=ru]"
  (format nil "{\"response\": ~A}"
          (json:encode-json-to-string
           (get-subways :city
                        (aif (hunchentoot:get-parameter "city") it "spb")
                        :lang
                        (aif (hunchentoot:get-parameter "lang") it "ru")))))

(defun get-restaurants (&key city greater-age below-age subways latitude longitude distance limit offset sort sort_type lang)
  (let ((rs (select-dao 'shop)))
    (when city
      (let ((city-id (if (typep city 'integer)
                         city
                         (let ((dao-obj-lst (select-dao 'city (:= 'code city))))
                           (if (null dao-obj-lst)
                               (return-from get-restaurants "city not found")
                               (id (car dao-obj-lst)))))))
        (setf rs (remove-if-not #'(lambda (x)
                                    (equal city-id (city-id x)))
                                rs))))
    (when greater-age
      (let ((timepoint (- (get-universal-time) (* 60 60 24 30 greater-age))))
        (setf rs (remove-if-not #'(lambda (x)
                                    (< timepoint (opening-date x)))
                                rs))))
    (when below-age
      (let ((timepoint (- (get-universal-time) (* 60 60 24 30 below-age))))
        (setf rs (remove-if-not #'(lambda (x)
                                    (> timepoint (opening-date x)))
                                rs))))
    ;; ...
    rs))


(defmethod to-json (lang)
  (list (cons :id 1)))



(print (let ((lang "ru")
      (item *makarena*))
  (list (cons :id (id item))
        (cons :name (car (mapcar #'val (remove-if-not #'(lambda (x)
                                                          (equal (lang-id x) (get-lang-id lang)))
                                                      (load-values (car (load-options item :name "name")))))))
        (cons :description (car (mapcar #'val (remove-if-not #'(lambda (x)
                                                                 (equal (lang-id x) (get-lang-id lang)))
                                                             (load-values (car (load-options item :name "description")))))))
        (cons :opening-date (opening-date item))
        (cons :price (price item))
        (cons :logo (logo item))
        (cons :photo (photo item))
        (cons :site (site item))
        (list :capacity (cons :indoor (indoor item)) (cons :outdoor (outdoor item)))
        `(:phone ,@(aif (load-options item :name "phone")
                        (mapcar #'(lambda (x)
                                            (cons (name x) (val (car (load-values x)))))
                                        (load-options item :parent-id (id (car it))))))
        `(:address ,(cons :latitude (latitude item))
                   ,(cons :longitude (longitude item))
                   ,(cons :distance "calculate value")
                   ,(cons :postal-code (postal-code item))
                   ,(cons :city-id (city-id item))
                   ,(cons :city-code (city-code item))
                   ;(:SUBWAY--ID . 1)
                   ,(cons :subway (list (mapcar #'(lambda (subway)
                                                    (car (mapcar #'val (remove-if-not #'(lambda (x)
                                                                                          (equal (lang-id x) (get-lang-id lang)))
                                                                                      (load-values (car (load-options subway :name "name")))))))
                                                (query-dao
                                                 'subway
                                                 (format nil "SELECT * FROM subway WHERE id IN (SELECT subway_id FROM shop_2_subway  WHERE shop_id = ~A)"
                                                         (id item))))))
                   ,(cons :street (car (mapcar #'val (remove-if-not #'(lambda (x)
                                                                        (equal (lang-id x) (get-lang-id lang)))
                                                                    (load-values (car (load-options item :name "street")))))))
                   ,(cons :building  (car (mapcar #'val (remove-if-not #'(lambda (x)
                                                                            (equal (lang-id x) (get-lang-id lang)))
                                                                       (load-values (car (load-options item :name "building"))))))))
        `(:estimate ,(cons :rating (rating item))
                    ,(cons :rating-count (rating-count item))
                    ,(cons :comment-count (comment-count item)))
        ;; (:WORKTIME (("12:00:00" "23:00:00"))
        ;;            (("08:00:00" "12:00:00") ("15:00:00" "23:00:00")) NIL
        ;;            (("08:00:00" "12:00:00") ("15:00:00" "22:00:00")) NIL NIL
        ;;            (("11:00:00" "23:00:00"))))
        )))





((:ID . 1) (:NAME . "Name")
 (:DESCRIPTION . "Description")
 (:OPENING--DATE . 1310786880)
 (:PRICE . 3)
 (:LOGO . "/images/restaurnt/1.jpg")
 (:PHOTO . "/images/restaurnt/1.jpg")
 (:SITE . "http://macarenabar.ru")
 (:CAPACITY (:INDOOR . "162")
            (:OUTDOOR . "40"))
 (:PHONE (:MAIN . "+78121112233")
         (:FAX . "")
         (:DELIVERY . "+78123332211")
         (:BANQUET . "+78123332211"))
 (:ADDRESS (:LATITUDE . 59.8236)
           (:LONGITUDE . 30.3373)
           (:DISTANCE . 10.4142)
           (:POSTAL--CODE . "307660")
           (:COUNTRY--ID . 1)
           (:CITY--ID . 2)
           (:SUBWAY--ID . 1)
           (:STREET . "Good street")
           (:BUILDING . "128"))
 (:ESTIMATE (:RATING . 3.34)
            (:RATING--COUNT . 96)
            (:COMMENT--COUNT . 89))
 (:WORKTIME (("12:00:00" "23:00:00"))
            (("08:00:00" "12:00:00") ("15:00:00" "23:00:00")) NIL
            (("08:00:00" "12:00:00") ("15:00:00" "22:00:00")) NIL NIL
            (("11:00:00" "23:00:00"))))

;; haversinus
;; http://js-php.ru/web-development/distance-from-dot-to-dot/
;; lat1=deg2rad(lat1);
;; lng1=deg2rad(lng1);
;; lat2=deg2rad(lat2);
;; lng2=deg2rad(lng2);
;; return Math.round( 6378137 * Math.acos( Math.cos( lat1 ) * Math.cos( lat2 ) * Math.cos( lng1 - lng2 ) + Math.sin( lat1 ) * Math.sin( lat2 ) ) );
(defun haversinus (latitude longitude)
  (let ((num (format nil "SQRT(POW(COS(RADIANS(latitude)) * SIN(ABS(RADIANS(~A)-RADIANS(longitude))),2) + POW(COS(RADIANS(~A)) * SIN(RADIANS(latitude)) - SIN(RADIANS(~A)) * COS(RADIANS(latitude))*COS(ABS(RADIANS(~A)-RADIANS(longitude))),2))'" longitude latitude latitude longitude))
        (den (format nil "SIN(RADIANS(latitude))*SIN(RADIANS(~A)) + COS(RADIANS(latitude))*COS(RADIANS(~A))*COS(ABS(RADIANS(~A)-RADIANS(longitude)))"
                     latitude latitude $longitude))
        (res (format nil "ATAN(~A/(~A)) * 6373" num den)))
    res))

(defun nearest (latitude_start longitude_start distance)
  (let ((haversinus (haversinus latitude_start longitude_start)))
    (format nil "SELECT *, ~A as distance FROM table  WHERE distance < ~A"
            haversinus distance)))


(get-restaurants :city "spb")


(restas:define-route api-lang ("/api.php")
  (let ((method (hunchentoot:get-parameter "method")))
    (cond ((equal method "getLanguages")
           (format nil "{\"response\": ~A}" (json:encode-json-to-string (get-languages :lang (aif (hunchentoot:get-parameter "lang") it "ru")))))
          ((equal method "getCountries")
           (format nil "{\"response\": ~A}" (json:encode-json-to-string (get-countryes :lang (aif (hunchentoot:get-parameter "lang") it "ru")))))
          ((equal method "location.getCities")
           (format nil "{\"response\": ~A}"
                   (json:encode-json-to-string
                    (get-cityes
                     :country (parse-integer (hunchentoot:get-parameter "country_id"))
                     :lang (aif (hunchentoot:get-parameter "lang") it "ru")))))
          ((equal method "location.getSubway")
           (format nil "{\"response\": ~A}"
                   (json:encode-json-to-string
                    (get-subways
                     :city (parse-integer (hunchentoot:get-parameter "city_id"))
                     :lang (aif (hunchentoot:get-parameter "lang") it "ru")))))
          (t "disp-error"))))


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



;; (restas:define-route resto-id ("/restaurant/:id/menu")
;;   (let ((rs)
;;         (lang (aif (hunchentoot:get-parameter "lang") it "ru"))
;;         (v (gethash (parse-integer id :junk-allowed t) *h-resto*))
;;         (categoryes)
;;         (products))
;;     ;; (setf rs (make-instance
;;     ;;           'category
;;     (format nil "
;; [
;;     {
;;         \"categories\" :
;;         {
;;             \"id\":\"1\",
;;             \"name\" : \"Акции\",
;;             \"icon\":\"/img/icon_action.png\"
;;         },
;;         {
;;             \"id\":\"2\",
;;             \"name\" : \"Специальные предложения\",
;;             \"icon\":\"/img/icon_spec.png\"
;;         }
;;     },
;;     {
;;         \"products\" :
;;         {
;;             \"id\":\"1\",
;;             \"name\" : \"Еда мужская, 1 кг.\",
;;             \"pic\":\"/img/eda.png\"
;;         }
;;     }
;; ]")))

