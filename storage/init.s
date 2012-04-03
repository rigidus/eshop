;;;; init
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


;; start
(restas:start '#:eshop.storage :port 8082)
(restas:debug-mode-on)
;; (restas:debug-mode-off)
(setf hunchentoot:*catch-errors-p* t)
