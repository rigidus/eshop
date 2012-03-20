;;;; classes.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop.storage)


;; redefining JSON::WRITE-JSON-CHARS in DEFUN
;; reason: cyrillic symbols modifying
(defun json::write-json-chars (s stream)
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



;; object address is part of object restaurant
(defclass address ()
  ((latitude
    :initarg :latitude
    :initform 0
    :accessor latitude
    :documentation "(type 'float)")
   (longitude
    :initarg :longitude
    :initform 0
    :accessor longitude
    :documentation "(:type 'float)")
   (postal_code
    :initarg :postal_code
    :initform ""
    :accessor postal_code
    :documentation "(:type 'string)")
   (country
    :initarg :country
    :initform ""
    :accessor country
    :documentation "(:type 'string)")
   (city
    :initarg :city
    :initform ""
    :accessor city
    :documentation "(:type 'string)")
   (subway
    :initarg :subway
    :initform ""
    :accessor subway
    :documentation "(:type 'string)")
   (street
    :initarg :street
    :initform ""
    :accessor street
    :documentation "(:type 'string)")
   (building
    :initarg :building
    :initform ""
    :accessor building
    :documentation "(:type 'string)")))


;; test constructor address
(defparameter *tmp*
  (make-instance
   'address
   :latitude 59.856727
   :longitude 23423.234
   ))



;; restaurant
(defclass restaurant ()
  ((id
    :initarg :id
    :initform (parse-integer (subseq (symbol-name (gensym)) 1))
    :accessor id
    :documentation "(:type 'integer)")
   (name
    :initarg :name
    :initform ""
    :accessor name
    :documentation "(:type 'string)")
   (description
    :initarg :description
    :initform ""
    :accessor description
    :documentation "(:type 'string)")
   (opening_date
    :initarg :opening_date
    :initform ""
    :accessor opening_date
    :documentation "(:type 'timestamp)")
   (price
    :initarg :price
    :initform 0
    :accessor price
    :documentation "(:type '(integer 1 5)")
   (photo
    :initarg :photo
    :initform ""
    :accessor photo
    :documentation "(:type 'pathname)")
   (site
    :initarg :site
    :initform ""
    :accessor site
    :documentation "(:type 'uri)")
   (phone
    :initarg :phone
    :initform ""
    :accessor phone
    :documentation "(:type '(plist (:main 'string
                                    :delivery 'string
                                    :bankuet 'string)))")
   (address
    :initarg :address
    :initform (make-instance 'address)
    :accessor address
    :documentation "(:type 'address")
   ;; (estimate
   ;;  :initarg :estimate
   ;;  :accessor estimate
   ;;  :documentation "(:type '(plist rating 'float
   ;;                                 rating_count 'CALC
   ;;                                 comment_count 'CALC))")
   ;; (worktime
   ;;  :initarg :worktime
   ;;  :accessor worktime
   ;;  :documentation "(:type '(list
   ;;                            (list \"07:00\" \"12:00\")
   ;;                            (list \"07:00\" \"12:00\")))")
   ;; (optional
   ;;  :initarg :optional
   ;;  :accessor optional
   ;;  :documentation "(:type 'alist)")
   ))



;; serialize address
(defparameter *tmp*
  (make-instance
   'restaurant
   ))


(print (encode-json-to-string *tmp*))



;; {
;; "id":1135,
;; "name":"",
;; "description":"",
;; "opening_date":"",
;; "price":0,
;; "photo":"",
;; "site":"",
;; "phone":"",
;; "address":{
;;  "latitude":0,
;;  "longitude":0,
;;  "postal_code":"",
;;  "country":"",
;;  "city":"",
;;  "subway":"",
;;  "street":"",
;;  "building":""}
;; }




(print
 (decode-json-from-string
  (encode-json-to-string
   `(( "kitchen"      . ,(encode-json-to-string '("мексиканская" "итальянская")))
     ( "service"      . ("завтрак" "ланч"))
     ( "additionally" . ("кальян"))
     ( "children"     . ("меню" "няня" "детская комната"))
     ( "music"        . ("живая"))
     ( "view"         . ("панорамный"))))))




;; (defparameter *tmp*
;;   (make-instance 'restaurant
;;                  :id 5
;;                  :name "makarena"
;;                  :description "Макарена"
;;                  :opening_date 32452345
;;                  :price 3
;;                  :photo "/images/restaurant/macarena.jpg"
;;                  :site "http://macarenabar.ru"
;;                  :phone '(( "main"      . "+78129063900")
;;                           ( "delivery"  . "+78129063900")
;;                           ( "bankuet"   . "+78129063900"))
;;                  ;; :address
;;                  ))

;; (print (decode-json-from-string (encode-json-to-string *tmp*)))


;; map on slots test

;; (mapcar #'(lambda (x)
;;             (print (slot-value *tmp*
;;                                (slot-definition-name x))))
;;         (compute-slots (find-class 'address)))
