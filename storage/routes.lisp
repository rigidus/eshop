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


(defun get-restaurants (&key (city "spb") subway_id age age_type latitude longitude distance limit offset sort sort_type lang)
  (let ((city-id (if (typep city 'integer)
                     city
                     (let ((dao-obj-lst (select-dao 'city (:= 'code city))))
                       (if (null dao-obj-lst)
                           (return-from get-subways "city not found")
                           (id (car dao-obj-lst)))))))
    'stub))

(restas:define-route api-lang ("/api.php")
  'stub)


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

