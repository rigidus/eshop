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
           "/lang [lang=ru]"
           "/country [lang=ru]"
           "/city [country=rus] [lang=ru]"
           "/subway [city=spb] [lang=ru]"
           "/restaurant [lang=ru]"
           "/restaurant/{code} [lang=ru]"
           "/category [restaurant] [lang=ru]"
           "/dish [restaurant] [category] [lang=ru]"
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
  "/lang [lang=ru]"
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
  "/country [lang=ru]"
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
  "/city [country=rus] [lang=ru]"
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
  "/subway [city=spb] [lang=ru]"
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


(defun get-shop (item &key (lang "ru"))
  (list (cons :id (id item))
        (cons :code (code item))
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
                   ,(cons :subway (mapcar #'(lambda (subway)
                                                    (car (mapcar #'val (remove-if-not #'(lambda (x)
                                                                                          (equal (lang-id x) (get-lang-id lang)))
                                                                                      (load-values (car (load-options subway :name "name")))))))
                                                (query-dao
                                                 'subway
                                                 (format nil "SELECT * FROM subway WHERE id IN (SELECT subway_id FROM shop_2_subway  WHERE shop_id = ~A)"
                                                         (id item)))))
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
        ))


(restas:define-route restaurant ("/restaurant")
  "/restaurant [lang=ru]"
  (format nil "{\"response\": ~A}"
          (json:encode-json-to-string
           (loop :for item :in (select-dao 'shop) :collect
              (get-shop item :lang (aif (hunchentoot:get-parameter "lang") it "ru"))))))

(restas:define-route restaurant-id ("/restaurant/:code")
  (format nil "{\"response\": ~A}"
          (json:encode-json-to-string
           (aif (car (select-dao 'shop (:= 'code code)))
                (get-shop it :lang (aif (hunchentoot:get-parameter "lang") it "ru"))))))



(defun get-category (item &key (lang "ru"))
  (list (cons :id (id item))
        (cons :code (code item))
        (cons :image (image item))
        (cons :parent (aif (get-dao 'category (parent-id item))
                           (code it)
                           nil))
        (cons :name (car (mapcar #'val (remove-if-not #'(lambda (x)
                                                          (equal (lang-id x) (get-lang-id lang)))
                                                      (load-values (car (load-options item :name "name")))))))
        (cons :subcategoryes (mapcar #'code (select-dao 'category (:= 'parent-id (id item)))))))

;; (get-category *hot-dishes*)

(restas:define-route category ("/category")
  (let ((rs (aif (hunchentoot:get-parameter "restaurant")
                 ;; isset parameter restaurant
                 (aif (select-dao 'shop (:= 'code it))
                      (query-dao 'category
                                 (format nil "SELECT * FROM category WHERE id IN (SELECT category_id FROM shop_2_category WHERE shop_id = ~A)"
                                         (id (car it)))))
                 ;; no parameter restaurant
                 (select-dao 'category))))
    (format nil "{\"response\": ~A}"
            (json:encode-json-to-string
             (loop :for item :in rs :collect (get-category item :lang (aif (hunchentoot:get-parameter "lang") it "ru")))))))

;; todo - завершить здесь представление
(defun get-product (item &key (lang "ru"))
  (list (cons :id (id item))
        (cons :code (code item))
        (cons :image (image item))
        (cons :parent (aif (get-dao 'category (parent-id item))
                           (code it)
                           nil))
        (cons :name (car (mapcar #'val (remove-if-not #'(lambda (x)
                                                          (equal (lang-id x) (get-lang-id lang)))
                                                      (load-values (car (load-options item :name "name")))))))
        (cons :subcategoryes (mapcar #'code (select-dao 'category (:= 'parent-id (id item)))))))


;; todo - оттестировать
(restas:define-route dish ("/dish")
  (let ((err) (rs))
    (aif (hunchentoot:get-parameter "restaurant")
         ;; isset parameter restaurant
         (aif (select-dao 'shop (:= 'code it))
              (setf rs (append rs (query (:select 'shop_id :from 'shop_2_product :where (:= 'shop_id (id (car it)))))))
              (push "restaurant not found" err)))
    (aif (hunchentoot:get-parameter "category")
         ;; isset parameter restaurant
         (aif (select-dao 'category (:= 'code it))
              (setf rs (append rs (query (:select 'product_id :from 'category_2_product :where (:= 'category_id (id (car it)))))))
              (push "category not found" err)))
    (when err
      (return-from dish (format nil "{\"errors\": ~A}" (json:encode-json-to-string err))))
    (format nil "{\"response\": ~A}"
            (json:encode-json-to-string
             (loop :for item :in (query-dao 'product (format nil "SELECT * FROM product WHERE id IN (~{~A~^, ~})" (remove-duplicates rs)))
                :collect (get-product item :lang (aif (hunchentoot:get-parameter "lang") it "ru")))))))


;; todo - оттестировать и интегрировать расчет расстояний в сферических координатах
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


(defparameter *savetest* nil)

(restas:define-route savetest ("/savetest" :method :post)
  (push (hunchentoot:post-parameters*) *savetest*))
