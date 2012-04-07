;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>


(in-package #:eshop.storage)


#| POSTGRESQL

вставить в /etc/postgresql/<version>/main/pg_hba.conf
local all all trust
чтобы он доверял локальным пользователям
потом переключаемся в пользователя postgres и создаем базу
createuser -DRS <dbuser>
createdb -l ru_RU.UTF-8 -T template0 -O <dbuser> <dbname>
psql
alter user <dbuser> with password '<dbpassword>';

|#

(defparameter *db-name* "restodb")
(defparameter *db-user* "resto")
(defparameter *db-pass* "resto1111")
(defparameter *db-serv* "localhost")
(defparameter *db-spec* (list *db-name* *db-user* *db-pass* *db-serv*))
(connect-toplevel *db-name* *db-user* *db-pass* *db-serv*)
(disconnect-toplevel)

;; (with-connection *db-spec*
;;   (query (:select '* :from 'product)))




;; produce (and re-init storage table if need) linktable object
(defmacro def~daoclass-linktable (src dst &optional re-init)
  (flet ((get-fld-str (fld)
           (let* ((fld-str      (format nil "~A" (symbol-name fld)))
                  (fld-id-symbol  (intern (format nil "~A-ID" fld-str)))
                  (fld-id-keyword   (intern (format nil "~A-ID" fld-str) :keyword)))
             `(,fld-id-symbol :col-type integer :initarg ,fld-id-keyword :initform 0 :accessor ,fld-id-symbol))))
    (let* ((class-name-symbol (intern (format nil "~A-2-~A" (symbol-name src) (symbol-name dst))))
           (class-definition  `(defclass ,class-name-symbol ()
                                 (,(get-fld-str src)
                                  ,(get-fld-str dst))
                                 (:metaclass dao-class)))
           (re-initialization `(progn
                                 ,class-definition
                                 (query (sql (:drop-table :if-exists ',class-name-symbol)))
                                 (execute (dao-table-definition ',class-name-symbol)))))
      (if (not re-init) class-definition re-initialization))))

;; linktable test
;; (print (macroexpand-1 '(def~daoclass-linktable shop option t)))


;; produce incrementor closure
(defmacro incrementor (name fld)
  `(let ((,(intern (format nil "INC-~A-~A" (symbol-name name) (symbol-name fld))) 0))
     (list
      (defun ,(intern (format nil "INCF-~A-~A" (symbol-name name) (symbol-name fld)())) ()
        (incf ,(intern (format nil "INC-~A-~A" (symbol-name name) (symbol-name fld)))))
      (defun ,(intern (format nil "INIT-~A-~A" (symbol-name name) (symbol-name fld) ())) (init-value)
        (setf ,(intern (format nil "INC-~A-~A" (symbol-name name) (symbol-name fld))) init-value)))))

;; incrementor test
;; (print (macroexpand-1 '(incrementor product id)))


;; produce (and re-init storage table if need) entity object
(defmacro def~daoclass-entity (name direct-superclasses direct-slots &rest options)
  (let ((incf)
        (re-init)
        (re-link)
        (new-options)
        (fields-definition  (loop :for slot :in direct-slots :collect
                               (let* ((slot-symbol (car slot))
                                      (slot-plist (cdr slot))
                                      (slot-result))
                                 (aif (getf slot-plist :writer)    (setf (getf slot-result :writer) it))
                                 (aif (getf slot-plist :reader)    (setf (getf slot-result :reader) it))
                                 (aif (getf slot-plist :accessor)
                                      (setf (getf slot-result :accessor) it)
                                      ;; else
                                      (unless (or (getf slot-plist :writer) (getf slot-plist :reader))
                                        (setf (getf slot-result :accessor) slot-symbol)))
                                 (aif (getf slot-plist :initform)  (setf (getf slot-result :initform) it))
                                 (aif (getf slot-plist :initarg)
                                      (setf (getf slot-result :initarg) it)
                                      (setf (getf slot-result :initarg) (intern (symbol-name slot-symbol) :keyword)))
                                 (aif (getf slot-plist :column)    (setf (getf slot-result :column) it))
                                 (aif (getf slot-plist :col-type)  (setf (getf slot-result :col-type) it))
                                 `(,slot-symbol ,@slot-result)))))
    (loop :for item :in options :do
       (case (car item)
         (:incf     (setf incf (cdr item)))
         (:re-init  (setf re-init t))
         (:re-link  (setf re-link t))
         (otherwise (push item new-options))))
    `(progn
       ,@(loop :for item :in incf :collect `(incrementor ,name ,item))
       (defclass ,name ,direct-superclasses ,fields-definition ,@(list* '(:metaclass dao-class) (reverse new-options)))
       ,(when re-init
              `(progn
                 (query (sql (:drop-table :if-exists ',name)))
                 (execute (dao-table-definition ',name))))
       ,(when re-link
              `(progn
                 (def~daoclass-linktable ,name option ,re-init)
                 (defmethod make-option ((dao-obj ,name) lang name &optional (optype "") (parent-id 0))
                   (let ((lang-id    (query (:select 'id :from 'lang :where (:= 'code lang)) :single))
                         (option     (make-dao 'option :lang lang :name name :optype optype :parent-id parent-id)))
                     (query (:insert-into ',(intern (format nil "~A-2-OPTION" (symbol-name name))) :set
                                          'lang-id lang-id
                                          'option-id (id option)))
                     option))
                 (defmethod load-options ((dao-obj ,name))
                   (loop :for item :in (mapcar #'car (query (:select 'option-id :from  ',(intern (format nil "~A-2-OPTION" (symbol-name name)))
                                                                     :where (:= ',(intern (format nil "~A-ID" (symbol-name name))) 1))))
                      :collect (initialize-instance (get-dao 'option item))))
                 (defmethod get-opts-val ((dao-obj ,name))
                   (mapcar #'(lambda (option)
                               (list
                                (name option)
                                (mapcar #'(lambda (optval)
                                            (val optval))
                                        (load-value option))))
                           (load-options dao-obj)))
                 ;; (defmethod get-option ((dao-obj ,name) lang name)
                 ;;   (cdr (assoc name (get-opts dao-obj lang) :test #'equal)))
                 )))))


;; entity test
(print (macroexpand-1 '(def~daoclass-entity product ()
                        ((id                :col-type integer         :initform (incf-product-id))
                         (category-id       :col-type integer         :initform 0)
                         (options                                     :initform ""))
                        (:keys id)
                        (:incf id)
                        (:re-init t)
                        (:re-link t))))



;; (defparameter *x* (make-dao 'product))
;; (id *x*)
;; (add-option *x* "ru" "qwe" "asd")
;; (add-option *x* "ru" "qwe123" "asd345")
;; (add-option *x* "en" "qwe123asd" "asd345qwe")
;; (get-opts *x* "en")
;; (get-opts *x* "ru")
;; (get-option *x* "ru" "qwe")

;;  LANG

(def~daoclass-entity lang ()
  ((id                :col-type integer         :initform (incf-lang-id))
   (code              :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t)
  (:re-link t))


;;  OPTVAL

(def~daoclass-entity optval ()
  ((id                :col-type integer         :initform (incf-optval-id))
   (option-id         :col-type integer         :initform 0)
   (lang-id           :col-type integer         :initform 0)
   (product-id        :col-type integer         :initform 0)
   (val               :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t))


;;  OPTION

(def~daoclass-entity option ()
  ((id                :col-type integer         :initform (incf-option-id))
   (parent-id         :col-type integer         :initform 0)
   (optype            :col-type string          :initform "")
   (lang-id           :col-type integer         :initform 0)
   (name              :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t))

(defmethod make-value ((dao-obj option) lang val &optional (product-id 0))
  (make-dao 'optval
            :option-id (id dao-obj)
            :lang-id (query (:select 'id :from 'lang :where (:= 'code lang)) :single)
            :val val
            :product-id product-id))

(defmethod load-value ((dao-obj option))
  (loop :for item :in (query-dao 'optval (:select '* :from 'optval :where (:= 'option-id (id dao-obj))))
     :collect (initialize-instance item)))

;; write to lang & lang_2_option
(defparameter *ru* (make-dao 'lang :code "ru"))
(make-option *ru* "ru" "test-name")
(make-option *ru* "ru" "test-name-2")
(defparameter *tmp* (make-option *ru* "ru" "test-name-3"))
(make-value *tmp* "ru" "test-value-3")
(make-value *tmp* "ru" "test-value-4")


(defun get-vals-by-option (optname-str)
  (awhen (find-if #'(lambda (dao-option)
                      (equal optname-str (name dao-option)))
                  (load-options *ru*))
    (load-value it)))

(mapcar #'(lambda (x)
            (val x))
        (get-vals-by-option "test-name-3"))




(add-option *ru* "ru" "name" "Русский")
(defparameter *en* (make-dao 'lang :code "en"))
(add-option *en* "ru" "name" "Английский")
(add-option *en* "en" "name" "English")
(add-option *ru* "en" "name" "Russian")


;; COUNTRY

(def~daoclass-entity country ()
  ((id                :col-type integer         :initform (incf-country-id))
   (code              :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t)
  (:re-link t))

(defparameter *rus* (make-dao 'country :code "rus"))
(add-option *rus* "ru" "name" "Россия")
(add-option *rus* "en" "name" "Russia")
(defparameter *usa* (make-dao 'country :code "usa"))
(add-option *usa* "ru" "name" "США")
(add-option *usa* "en" "name" "USA")


;; CITY
(def~daoclass-entity city ()
  ((id                :col-type integer         :initform (incf-city-id))
   (country-id        :col-type integer         :initform 0)
   (country-code      :col-type string          :initform "")
   (code              :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t)
  (:re-link t))



(defparameter *spb* (make-dao 'city :country-id (id *rus*) :country-code (code *rus*) :code "spb"))
(add-option *spb* "ru" "name" "Санкт-Петербург")
(add-option *spb* "en" "name" "St.Peterburg")
(defparameter *mos* (make-dao 'city :country-id (id *rus*) :country-code (code *rus*) :code "mos"))
(add-option *mos* "ru" "name" "Москва")
(add-option *mos* "en" "name" "Moscow")
(defparameter *jfk* (make-dao 'city :country-id (id *usa*) :country-code (code *usa*) :code "jfk"))
(add-option *jfk* "ru" "name" "Нью-Йорк")
(add-option *jfk* "en" "name" "New York")


;; SUBWAY
(def~daoclass-entity subway ()
  ((id                :col-type integer         :initform (incf-subway-id))
   (city-id           :col-type integer         :initform 0)
   (city-code         :col-type string          :initform "")
   (code              :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t)
  (:re-link t))

(defparameter *avto* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "avto"))
(add-option *avto* "ru" "name" "Автово")
(add-option *avto* "en" "name" "Avtovo")
(defparameter *narv* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "narv"))
(add-option *narv* "ru" "name" "Нарвская")
(add-option *narv* "en" "name" "Narvskaya")


;;  SHOP

(def~daoclass-entity shop ()
  ((id                :col-type integer         :initform (incf-shop-id))
   (opening-date      :col-type bigint          :initform 0)
   (price             :col-type integer         :initform 0)
   (logo              :col-type string          :initform "")
   (photo             :col-type string          :initform "")
   (site              :col-type string          :initform "")
   (indoor            :col-type integer         :initform 0)
   (outdoor           :col-type integer         :initform 0)
   (latitude          :col-type float           :initform 0.0)
   (longitude         :col-type float           :initform 0.0)
   (postal-code       :col-type string          :initform 0.0)
   (city-id           :col-type float           :initform 0.0)
   (city-code         :col-type string          :initform "")
   (rating            :col-type float           :initform 0.0)
   (rating-count      :col-type integer         :initform 0)
   (comment-count     :col-type integer         :initform 0)
   (worktime          :col-type string          :initform ""))
  ;; name description phone subways street building
  (:keys id)
  (:incf id)
  (:re-init t)
  (:re-link t))

(def~daoclass-linktable shop subway t)


;; decoded time
(multiple-value-bind (second minute hour date month year)
    (decode-universal-time (get-universal-time))
  (format nil "~2,'0d.~2,'0d.~d" date month year))

(defparameter *makarena*
  (make-dao
   'shop
   :opening-date (get-universal-time)
   :price 3
   :logo "/images/restaurant/1.jpg"
   :photo "/images/restaurnt/1.jpg"
   :site "http://macarenabar.ru"
   :indoor 162
   :outdoor 40
   :latitude 59.8236
   :longitude 30.3373
   :postal-code "307660"
   :city-id (id *spb*)
   :city-code (code *spb*)
   :rating 3.34
   :rating-count 96
   :comment-count 89
   ;; :worktime '((("12:00" "23:00"))
   ;;             (("08:00" "12:00")("15:00" "23:00"))
   ;;             nil
   ;;             (("08:00" "12:00")("15:00" "22:00"))
   ;;             nil
   ;;             nil
   ;;             (("11:00" "23:00")))
   ))
(add-option *makarena* "ru" "name" "Макарена")
(add-option *makarena* "en" "name" "Makarena")
;; name description phone subways street building
(add-option *makarena* "ru" "descr" "Мы очень любим вкусно есть, вкусно пить и душевно общаться. Этим мы занимались последние несколько лет в 7 странах и более чем в 300 ресторанах. Все эти годы мы не просто наслаждались, мы вынашивали наш проект. Проект, в котором объединено все самое вкусное и интересное, что нам самим удалось попробовать в Испании, Италии, Португалии, Мексике, странах Латинской Америки и Средней Азии. Мы рады, что теперь у нас есть возможность поделиться всем этим с Вами в Санкт-Петербурге (СПб).")
(add-option *makarena* "en" "descr" "we are ...")
(add-option *makarena* "ru" "phone-main" "+78129063900")
(add-option *makarena* "ru" "phone-delivery" "+78129063900")
(query (:insert-into 'shop_2_subway :set 'shop-id (id *makarena*) 'subway_id (id *avto*)))
(query (:insert-into 'shop_2_subway :set 'shop-id (id *makarena*) 'subway_id (id *narv*)))
(add-option *makarena* "ru" "street" "Московский проспект")
(add-option *makarena* "en" "street" "Moscowsky prospect")
(add-option *makarena* "ru" "building" "206")
(add-option *makarena* "en" "building" "206")
(add-option *makarena* "en" "building" "206")

 ;; :optional (make-instance
 ;;            'optional
 ;;            :kitchen '("мексиканская" "итальянская")
 ;;            :service '("завтрак" "ланч")
 ;;            :additionally '("кальян")
 ;;            :children '("меню" "няня" "детская комната")
 ;;            :music '("живая")
 ;;            :view '("панорамный"))
 ;; ))


(get-opts *makarena* "ru")
