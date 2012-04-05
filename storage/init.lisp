;;;; init.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop.storage)


(defparameter *h-country* (make-hash-table :test #'equal))

(setf (gethash "ru" *h-country*)
      (make-i18n-str :ru "Россия" :en "Russia"))



(defparameter *h-city* (make-hash-table :test #'equal))

(setf (gethash "mos" *h-city*)
      (make-i18n-str :ru "Москва" :en "Moscow"))
(setf (gethash "spb" *h-city*)
      (make-i18n-str :ru "Санкт-Петербург" :en "St. Petersburg"))



(defparameter *h-resto* (make-hash-table))

(setf (gethash 1 *h-resto*)
      (make-instance
       'resto
       :id 1
       :name (make-i18n-str :ru "Макарена" :en "Macarena")
       :descr (make-i18n-str
               :ru "Мы очень любим вкусно есть, вкусно пить и душевно общаться. Этим мы занимались последние несколько лет в 7 странах и более чем в 300 ресторанах. Все эти годы мы не просто наслаждались, мы вынашивали наш проект. Проект, в котором объединено все самое вкусное и интересное, что нам самим удалось попробовать в Испании, Италии, Португалии, Мексике, странах Латинской Америки и Средней Азии. Мы рады, что теперь у нас есть возможность поделиться всем этим с Вами в Санкт-Петербурге (СПб)."
               :en "we ...")
       :price 3
       :photo "/images/rest/macarena.jpg"
       :site "http://macarenabar.ru"
       :phone (make-instance
               'phone
               :main "+78129063900"
               :delivery "+78129063900"
               :banquet "+78129063900")
       :address (make-instance
                 'address
                 :latitude 59.856727
                 :longitude 30.321833
                 :postal_code "196066"
                 :country "ru"
                 :city "spb"
                 :subway (make-i18n-str :ru "Московская" :en "Moscowskaya")
                 :street (make-i18n-str :ru "Московский проспект" :en "Moscowsky prospect")
                 :building "206")
       :estimate (make-instance
                  'estimate
                  :rating 4.98
                  :rating_count 27
                  :comment_count 7)
       :capacity (make-instance
                  'capacity
                  :indoor 50
                  :outdoor 30)
       :optional (make-instance
                  'optional
                  :kitchen '("мексиканская" "итальянская")
                  :service '("завтрак" "ланч")
                  :additionally '("кальян")
                  :children '("меню" "няня" "детская комната")
                  :music '("живая")
                  :view '("панорамный"))
       ))


(setf (gethash 2 *h-resto*)
      (make-instance
       'resto
       :id 2
       :name (make-i18n-str :ru "Тепло" :en "Teplo")
       :descr (make-i18n-str
               :ru "ru-descr"
               :en "en-descr")
       :price 4
       :photo "/images/rest/teplo.jpg"
       :address (make-instance
                 'address
                 :latitude 59.931887
                 :longitude 30.305412
                 :postal_code "190000"
                 :country "ru"
                 :city "spb"
                 :subway (make-i18n-str :ru "Невский проспект" :en "Nevsky prospect")
                 :street (make-i18n-str :ru "Большая Морская" :en "Bolshaya Morskaya")
                 :building "45")
       :estimate (make-instance
                  'estimate
                  :rating 4.58
                  :rating_count 269
                  :comment_count 54)))


(setf (gethash 3 *h-resto*)
      (make-instance
       'resto
       :id 3
       :name (make-i18n-str :ru "Гости" :en "Gosti")
       :descr (make-i18n-str
               :ru "ru-descr"
               :en "en-descr")
       :price 3
       :photo "/images/rest/gosti.jpg"
       :address (make-instance
                 'address
                 :latitude 59.935169
                 :longitude 30.316003
                 :postal_code "190000"
                 :country "ru"
                 :city "spb"
                 :subway (make-i18n-str :ru "Невский проспект" :en "Nevsky prospect")
                 :street (make-i18n-str :ru "Большая Морская" :en "Bolshaya Morskaya")
                 :building "13/8")
       :estimate (make-instance
                  'estimate
                  :rating 4.1
                  :rating_count 22
                  :comment_count 77)))

(setf (gethash 4 *h-resto*)
      (make-instance
       'resto
       :id 4
       :name (make-i18n-str :ru "Антрекот" :en "Antrecot")
       :descr (make-i18n-str
               :ru "ru-descr"
               :en "en-descr")
       :price 3
       :photo "/images/rest/antrekot.jpg"
       :address (make-instance
                 'address
                 :latitude 59.934281
                 :longitude 30.312338
                 :postal_code "190000"
                 :country "ru"
                 :city "spb"
                 :subway (make-i18n-str :ru "Невский проспект" :en "Nevsky prospect")
                 :street (make-i18n-str :ru "Большая Морская" :en "Bolshaya Morskaya")
                 :building "25")
       :estimate (make-instance
                  'estimate
                  :rating 3.8
                  :rating_count 17
                  :comment_count 23)))


(setf (gethash 5 *h-resto*)
      (make-instance
       'resto
       :id 5
       :name (make-i18n-str :ru "Италия" :en "Italia")
       :descr (make-i18n-str
               :ru "ru-descr"
               :en "en-descr")
       :price 3
       :photo "/images/rest/default.jpg"
       :address (make-instance
                 'address
                 :latitude 59.929605
                 :longitude 30.374600
                 :postal_code "190000"
                 :country "ru"
                 :city "spb"
                 :subway (make-i18n-str :ru "Площадь Восстания" :en "Ploshad Vosstania")
                 :street (make-i18n-str :ru "проспект Бакунина" :en "prospect Bakunina")
                 :building "5")
       :estimate (make-instance
                  'estimate
                  :rating 4.7
                  :rating_count 61
                  :comment_count 1)))


(setf (gethash 6 *h-resto*)
      (make-instance
       'resto
       :id 6
       :name (make-i18n-str :ru "Барбареско" :en "Barbaresco")
       :descr (make-i18n-str
               :ru "ru-descr"
               :en "en-descr")
       :price 4
       :photo "/images/rest/default.jpg"
       :address (make-instance
                 'address
                 :latitude 59.940262
                 :longitude 30.327807
                 :postal_code "190000"
                 :country "ru"
                 :city "spb"
                 :subway (make-i18n-str :ru "Невский проспект" :en "Nevsky prospect")
                 :street (make-i18n-str :ru "Конюшенная площадь" :en "Konushennaya ploshad")
                 :building "2")
       :estimate (make-instance
                  'estimate
                  :rating 4.6
                  :rating_count 38
                  :comment_count 1)))


;; start
(restas:start '#:eshop.storage :port 8082)
(restas:debug-mode-on)
;; (restas:debug-mode-off)
(setf hunchentoot:*catch-errors-p* t)
