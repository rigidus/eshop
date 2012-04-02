;;;; classes.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop.storage)


;; redefining JSON::WRITE-JSON-CHARS in DEFUN
;; reason: cyrillic symbols modifying
json::(defun write-json-chars (s stream)
  "Write JSON representations (chars or escape sequences) of
characters in string S to STREAM."
  (loop for ch across s
     for code = (char-code ch)
     with special
     if (setq special (car (rassoc ch json::+json-lisp-escaped-chars+)))
     do (write-char #\\ stream) (write-char special stream)
     else if (< #x1f code #x7f)
     do (write-char ch stream)
     else
     do (let ((special '#.(rassoc-if #'consp json::+json-lisp-escaped-chars+)))
          (destructuring-bind (esc . (width . radix)) special
            ;; (format stream "\\~C~V,V,'0R" esc radix width code)
            (write-char ch stream)
            ))))

;; redefining json::+json-lisp-escaped-chars+
(setf json::+json-lisp-escaped-chars+
      '((#\" . #\") (#\\ . #\\) #|(#\/ . #\/)|# (#\b . #\Backspace) (#\f . #\Page)
        (#\n . #\Newline) (#\r . #\Return) (#\t . #\Tab) (#\u 4 . 16)))



;; i18n structure definition
(defstruct i18n-str (ru "") (en "") (it ""))
;; (defparameter *tmp* (make-i18n-str :ru "цуацуа" :en "wwege"))
;; (i18n-str-ru *tmp*)



;; object phone is part of object resto and other objects
(defclass phone ()
  ((main              :initarg :main            :initform ""        :accessor main)
   (delivery          :initarg :delivery        :initform ""        :accessor delivery)
   (banquet           :initarg :banquet         :initform ""        :accessor banquet)))


;; object address is part of object resto
(defclass address ()
  ((latitude          :initarg :latitude        :initform 0         :accessor latitude)
   (longitude         :initarg :longitude       :initform 0         :accessor longitude)
   (postal_code       :initarg :postal_code     :initform ""        :accessor postal_code)
   (country           :initarg :country         :initform "ru"      :accessor country)      ;; Code of [Country]
   (city              :initarg :city            :initform "spb"     :accessor city)         ;; Code of [City]
   (subway            :initarg :subway          :initform ""        :accessor subway)
   (street            :initarg :street          :initform ""        :accessor street)
   (building          :initarg :building        :initform ""        :accessor building)))


;; object estimate is part of object resto
(defclass estimate ()
  ((rating            :initarg :rating          :initform ""        :accessor rating)
   (rating_count      :initarg :rating_count    :initform ""        :accessor rating_count)
   (comment_count     :initarg :comment_count   :initform ""        :accessor comment_count)))


;; object capacity is part of object resto
(defclass capacity ()
  ((indoor            :initarg :indoor          :initform ""        :accessor indoor)
   (outdoor           :initarg :outdoor         :initform ""        :accessor outdoor)))


;; optional
(defclass optional ()
  ((kitchen           :initarg :kitchen         :initform nil       :accessor kitchen)
   (service           :initarg :service         :initform nil       :accessor service)
   (additionally      :initarg :additionally    :initform nil       :accessor additionally)
   (children          :initarg :children        :initform nil       :accessor children)
   (music             :initarg :music           :initform nil       :accessor music)
   (view              :initarg :view            :initform nil       :accessor view)))


;; resto
(defclass resto ()
  ((id                :initarg :id              :initform 0         :accessor id)
   (name              :initarg :name            :initform ""        :accessor name)
   (descr             :initarg :descr           :initform ""        :accessor descr)
   (opening_date      :initarg :opening_date    :initform ""        :accessor opening_date)
   (price             :initarg :price           :initform ""        :accessor price)
   (photo             :initarg :photo           :initform ""        :accessor photo)
   (site              :initarg :site            :initform ""        :accessor site)
   (phone             :initarg :phone           :initform ""        :accessor phone)
   (address           :initarg :address         :initform ""        :accessor address)
   (estimate          :initarg :estimate        :initform ""        :accessor estimate)
   (capacity          :initarg :capacity        :initform ""        :accessor capacity)
   (worktime          :initarg :worktime        :initform ""        :accessor worktime)
   (optional          :initarg :optional        :initform ""        :accessor optional)))


(defclass resto~shortlist ()
  ((id                :initarg :id              :initform 0         :accessor id)
   (name              :initarg :name            :initform ""        :accessor name)
   (price             :initarg :price           :initform ""        :accessor price)
   (photo             :initarg :photo           :initform ""        :accessor photo)
   (address           :initarg :address         :initform ""        :accessor address)
   (estimate          :initarg :estimate        :initform ""        :accessor estimate)
   ))


(defclass resto~longview ()
  ((id                :initarg :id              :initform 0         :accessor id)
   (name              :initarg :name            :initform ""        :accessor name)
   (descr             :initarg :descr           :initform ""        :accessor descr)
   (opening_date      :initarg :opening_date    :initform ""        :accessor opening_date)
   (price             :initarg :price           :initform ""        :accessor price)
   (photo             :initarg :photo           :initform ""        :accessor photo)
   (site              :initarg :site            :initform ""        :accessor site)
   (phone             :initarg :phone           :initform ""        :accessor phone)
   (address           :initarg :address         :initform ""        :accessor address)
   (estimate          :initarg :estimate        :initform ""        :accessor estimate)
   (capacity          :initarg :capacity        :initform ""        :accessor capacity)
   (worktime          :initarg :worktime        :initform ""        :accessor worktime)
   (optional          :initarg :optional        :initform ""        :accessor optional)))




(defclass product ()
  ((id                :col-type integer         :initarg :id              :initform 0         :accessor id)
   (name              :col-type string          :initarg :name            :initform ""        :accessor name)
   (price             :col-type integer         :initarg :price           :initform ""        :accessor price)
   (opts                                        :initarg :opts            :initform ""        :accessor opts))
  (:metaclass dao-class)
  (:keys id))


(defclass optname ()
  ((name              :initarg :name            :initform ""        :accessor name)
   (value             :initarg :value           :initform ""        :accessor value)
   (optype            :initarg :optype          :initform nil       :accessor optype)
   (optgrp            :initarg :optgrp          :initform ""        :accessor optgrp))
  (:metaclass dao-class)
  (:keys name value optype optgrp))


(defclass optval ()
  ((product-id        :initarg :product-id      :initform ""        :accessor product-id)
   (option-id         :initarg :option-id       :initform ""        :accessor option-id)
   (value             :initarg :value           :initform ""        :accessor value))
  (:metaclass dao-class)
  (:keys product-id option-id value))





;; map on slots test

;; (mapcar #'(lambda (x)
;;             (print (slot-value *tmp*
;;                                (slot-definition-name x))))
;;         (compute-slots (find-class 'address)))

