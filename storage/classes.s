;;;; classes
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



(defclass phone ()
  ((main              :initarg :main            :initform ""        :accessor main)
   (delivery          :initarg :delivery        :initform ""        :accessor delivery)
   (banquet           :initarg :banquet         :initform ""        :accessor banquet)))


(defclass address ()
  ((latitude          :initarg :latitude        :initform 0         :accessor latitude)
   (longitude         :initarg :longitude       :initform 0         :accessor longitude)
   (postal_code       :initarg :postal_code     :initform ""        :accessor postal_code)
   (country           :initarg :country         :initform "ru"      :accessor country)      ;; Code of [Country]
   (city              :initarg :city            :initform "spb"     :accessor city)         ;; Code of [City]
   (subway            :initarg :subway          :initform ""        :accessor subway)
   (street            :initarg :street          :initform ""        :accessor street)
   (building          :initarg :building        :initform ""        :accessor building)))


(defclass estimate ()
  ((rating            :initarg :rating          :initform ""        :accessor rating)
   (rating_count      :initarg :rating_count    :initform ""        :accessor rating_count)
   (comment_count     :initarg :comment_count   :initform ""        :accessor comment_count)))




