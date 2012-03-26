;;;; init.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop.storage)


(defparameter *h-restaurant* (make-hash-table))


(setf (gethash 1 *h-restaurant*)
      (make-instance
       'restaurant
       :id 1
       :name "Macarena"
       :price 3
       :photo "/images/restaurant/macarena.jpg"
       :address (make-instance
                 'address
                 :latitude 59.856727
                 :longitude 30.321833
                 :postal_code "196066"
                 :country "Россия"
                 :city "Санкт-Петербург"
                 :subway "Московская"
                 :street "Московский проспект"
                 :building "206")
       :estimate (make-instance
                  'estimate
                  :rating 4.98
                  :rating_count 27
                  :comment_count 7)))


(setf (gethash 2 *h-restaurant*)
      (make-instance
       'restaurant
       :id 2
       :name "Тепло"
       :price 4
       :photo "/images/restaurant/teplo.jpg"
       :address (make-instance
                 'address
                 :latitude 59.931887
                 :longitude 30.305412
                 :postal_code "190000"
                 :country "Россия"
                 :city "Санкт-Петербург"
                 :subway "Невский проспект"
                 :street "Большая Морская"
                 :building "45")
       :estimate (make-instance
                  'estimate
                  :rating 4.58
                  :rating_count 269
                  :comment_count 54)))


(setf (gethash 3 *h-restaurant*)
      (make-instance
       'restaurant
       :id 3
       :name "Гости"
       :price 3
       :photo "/images/restaurant/gosti.jpg"
       :address (make-instance
                 'address
                 :latitude 59.935169
                 :longitude 30.316003
                 :postal_code "190000"
                 :country "Россия"
                 :city "Санкт-Петербург"
                 :subway "Невский проспект"
                 :street "Большая Морская"
                 :building "13/8")
       :estimate (make-instance
                  'estimate
                  :rating 4.1
                  :rating_count 22
                  :comment_count 77)))

(setf (gethash 4 *h-restaurant*)
      (make-instance
       'restaurant
       :id 4
       :name "Антрекот"
       :price 3
       :photo "/images/restaurant/antrekot.jpg"
       :address (make-instance
                 'address
                 :latitude 59.934281
                 :longitude 30.312338
                 :postal_code "190000"
                 :country "Россия"
                 :city "Санкт-Петербург"
                 :subway "Невский проспект"
                 :street "Большая Морская"
                 :building "25")
       :estimate (make-instance
                  'estimate
                  :rating 3.8
                  :rating_count 17
                  :comment_count 23)))


(setf (gethash 5 *h-restaurant*)
      (make-instance
       'restaurant
       :id 5
       :name "Italia"
       :price 3
       :photo "/images/restaurant/default.jpg"
       :address (make-instance
                 'address
                 :latitude 59.929605
                 :longitude 30.374600
                 :postal_code "190000"
                 :country "Россия"
                 :city "Санкт-Петербург"
                 :subway "Площадь Восстания"
                 :street "проспект Бакунина"
                 :building "5")
       :estimate (make-instance
                  'estimate
                  :rating 4.7
                  :rating_count 61
                  :comment_count 1)))


(setf (gethash 6 *h-restaurant*)
      (make-instance
       'restaurant
       :id 6
       :name "Barbaresco"
       :price 4
       :photo "/images/restaurant/default.jpg"
       :address (make-instance
                 'address
                 :latitude 59.940262
                 :longitude 30.327807
                 :postal_code "190000"
                 :country "Россия"
                 :city "Санкт-Петербург"
                 :subway "Невский проспект"
                 :street "Конюшенная площадь"
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
