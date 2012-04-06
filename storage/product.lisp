;;;; product.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Authors:
;;;;   Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>
;;;;   Fedorov Evgeniy aka Zhef

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

(with-connection *db-spec*
  (query (:select '* :from 'product)))




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
(print (macroexpand-1 '(def~daoclass-linktable shop option t)))
;; (PROGN
;;   (DEFCLASS SHOP-2-OPTION NIL
;;     ((SHOP-ID   :COL-TYPE INTEGER :INITARG :SHOP-ID   :INITFORM 0  :ACCESSOR SHOP-ID)
;;      (OPTION-ID :COL-TYPE INTEGER :INITARG :OPTION-ID :INITFORM 0  :ACCESSOR OPTION-ID))
;;     (:METACLASS DAO-CLASS))
;;   (QUERY (SQL (:DROP-TABLE :IF-EXISTS 'SHOP-2-OPTION)))
;;   (EXECUTE (DAO-TABLE-DEFINITION 'SHOP-2-OPTION)))


;; produce incrementor closure
(defmacro incrementor (name fld)
  `(let ((,(intern (format nil "INC-~A-~A" (symbol-name name) (symbol-name fld))) 0))
     (list
      (defun ,(intern (format nil "INCF-~A-~A" (symbol-name name) (symbol-name fld)())) ()
        (incf ,(intern (format nil "INC-~A-~A" (symbol-name name) (symbol-name fld)))))
      (defun ,(intern (format nil "INIT-~A-~A" (symbol-name name) (symbol-name fld) ())) (init-value)
        (setf ,(intern (format nil "INC-~A-~A" (symbol-name name) (symbol-name fld))) init-value)))))

;; incrementor test
(print (macroexpand-1 '(incrementor product id)))
;; (LET ((INC-PRODUCT-ID))
;;   (LIST
;;     (DEFUN INCF-PRODUCT-ID ()
;;       (INCF INC-PRODUCT-ID))
;;     (DEFUN INIT-PRODUCT-ID (INIT-VALUE)
;;       (SETF INC-PRODUCT-ID INIT-VALUE))))



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
                 (defmethod add-option ((dao-obj ,name) lang name &optional (value t))
                   (let ((lang-id    (query (:select 'id :from 'lang :where (:= 'code lang)) :single))
                         (option-id  (id (make-dao 'option :optype (symbol-name ',name)))))
                     (make-dao   'optname  :option-id option-id :lang-id lang-id :val name)
                     (when value
                       (make-dao 'optvalue :option-id option-id :lang-id lang-id :val value))
                     (query (:insert-into ',(intern (format nil "~A-2-OPTION" (symbol-name name))) :set
                                          ',(intern (format nil "~A-ID" name)) (id dao-obj)
                                          'option-id option-id)))))))))

;; entity test
(print (macroexpand-1 '(def~daoclass-entity product ()
                        ((id                :col-type integer         :initform (incf-product-id))
                         (category-id       :col-type integer         :initform 0)
                         (options                                     :initform ""))
                        (:keys id)
                        (:incf id)
                        (:re-init t)
                        (:re-link t))))


;; (PROGN
;;   (INCREMENTOR PRODUCT ID)
;;   (DEFCLASS PRODUCT NIL
;;     ((ID          :COL-TYPE INTEGER :INITARG :ID          :INITFORM (INCF-PRODUCT-ID) :ACCESSOR ID)
;;      (CATEGORY-ID :COL-TYPE INTEGER :INITARG :CATEGORY-ID :INITFORM 0                 :ACCESSOR CATEGORY-ID)
;;      (OPTIONS                       :INITARG :OPTIONS     :INITFORM ""                :ACCESSOR OPTIONS))
;;     (:METACLASS DAO-CLASS)
;;     (:KEYS ID))
;;   (PROGN
;;     (QUERY (SQL (:DROP-TABLE :IF-EXISTS 'PRODUCT)))
;;     (EXECUTE (DAO-TABLE-DEFINITION 'PRODUCT)))
;;   (PROGN
;;     (DEF~DAOCLASS-LINKTABLE PRODUCT OPTION T)
;;     (DEFMETHOD ADD-OPTION ((DAO-OBJ PRODUCT) LANG NAME &OPTIONAL (VALUE T))
;;       (LET ((LANG-ID    (QUERY (:SELECT 'ID :FROM 'LANG :WHERE (:= 'CODE LANG)) :SINGLE))
;;             (OPTION-ID  (ID (MAKE-DAO 'OPTION :OPTYPE (SYMBOL-NAME 'PRODUCT)))))
;;         (MAKE-DAO 'OPTNAME :OPTION-ID OPTION-ID :LANG-ID LANG-ID :VAL NAME)
;;         (WHEN VALUE
;;           (MAKE-DAO 'OPTVALUE :OPTION-ID OPTION-ID :LANG-ID LANG-ID :VAL VALUE))
;;         (QUERY (:INSERT-INTO 'PRODUCT-2-OPTION :SET
;;                              'PRODUCT-ID (ID DAO-OBJ)
;;                              'OPTION-ID OPTION-ID))))))



;;  OPTNAME

(def~daoclass-entity optname ()
  ((option-id         :col-type integer         :initform 0)
   (lang-id           :col-type integer         :initform 0)
   (val               :col-type string          :initform ""))
  (:re-init t))


;;  OPTVALUE

(def~daoclass-entity optvalue ()
  ((option-id         :col-type integer         :initform 0)
   (lang-id           :col-type integer         :initform 0)
   (val               :col-type string          :initform "")
   (product-id        :col-type integer         :initform 0))
  (:re-init t))


;;  OPTION

(def~daoclass-entity option ()
  ((id                :col-type integer         :initform (incf-option-id))
   (parent-id         :col-type integer         :initform 0)
   (optype            :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t))




;;  LANG

(def~daoclass-entity lang ()
  ((id                :col-type integer         :initform (incf-lang-id))
   (code              :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t)
  (:re-link t))


(let ((ru-lang-id (id (make-dao 'lang :code "ru"))))
  (lang-add-option ru-lang-id "ru" "Язык" "Русский")
  (let ((en-lang-id (id (make-dao 'lang :code "en"))))
    (lang-add-option en-lang-id "ru" "Язык" "Английский")
    (lang-add-option en-lang-id "en" "Language" "English")
    (lang-add-option ru-lang-id "en" "Language" "Russian")))

;; TODO: Получение в i18str



;; COUNTRY

(def~daoclass-entity country ()
  ((id                :col-type integer         :initform (incf-country-id))
   (code              :col-type string          :initform "")
  (:keys id)
  (:incf id)
  (:re-init t))

(make-dao 'country :code "rus" :name "Россия")
(make-dao 'country :code "ita" :name "Italy")


;; CITY
(def~daoclass-entity city ()
  ((id                :col-type integer         :initform (incf-city-id))
   (country-id        :col-type integer         :initform 0)
   (name              :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t))

(make-dao 'city :country-id 1 :name "Санкт-Петербург")
(make-dao 'city :country-id 1 :name "Москва")
(make-dao 'city :country-id 2 :name "Rome")


;; SUBWAY
(def~daoclass-entity subway ()
  ((id                :col-type integer         :initform (incf-subway-id))
   (sity-id           :col-type integer         :initform 0)
   (name              :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t))

(make-dao 'city :country-id 1 :name "Санкт-Петербург")
(make-dao 'city :country-id 1 :name "Москва")
(make-dao 'city :country-id 2 :name "Rome")




;;  PRODUCT

(def~daoclass-entity product ()
  ((id                :col-type integer         :initform (incf-product-id))
   (category-id       :col-type integer         :initform 0)
   (options                                     :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t))


;;  PRODUCT-2-OPTION

(def~daoclass-linktable product option t)


;;  CATEGORY

(def~daoclass-entity category ()
  ((id                :col-type integer         :initform (incf-category-id))
   (parent-id         :col-type integer         :initform 0)
   (shop-id           :col-type integer         :initform 0))
  (:keys id)
  (:incf id)
  (:re-init t))


;;  CATEGORY-2-OPTION

(def~daoclass-linktable category option t)


;;  SHOP

(def~daoclass-entity shop ()
  ((id                :col-type integer         :initform (incf-shop-id)))
  (:keys id)
  (:incf id)
  (:re-init t))

;;  SHOP-2-OPTION

(def~daoclass-linktable shop option t)


;;  SHOP-2-CATEGORY

(def~daoclass-linktable shop category t)









;;  API : SHOP

(defun create-shop ()
  (id (make-dao 'shop)))

(defun shop-add-option (shop-id lang name &optional value)
  (let ((shop        (get-dao   'shop shop-id))
        (lang-id     (query     (:select 'id :from 'lang :where (:= 'val lang)) :single)))
    (let ((option-id (id (make-dao  'option :optype "shop-option"))))
      (make-dao 'optname
                :option-id option-id
                :lang-id lang-id
                :val name)
      (when value
        (make-dao 'optvalue
                  :option-id option-id
                  :lang-id lang-id
                  :val value))
      t)))


;; API : CATEGORY

(defun create-category ()
  (id (make-dao 'category)))

(defun category-add-option (category-id lang name &optional value)
  (let ((category    (get-dao   'category category-id))
        (lang-id     (query     (:select 'id :from 'lang :where (:= 'val lang)) :single)))
    (let ((option-id (id (make-dao  'option :optype "category-option"))))
      (make-dao 'optname
                :option-id option-id
                :lang-id lang-id
                :val name)
      (when value
        (make-dao 'optvalue
                  :option-id option-id
                  :lang-id lang-id
                  :val value))
      t)))

(defun addlink-category-2-shop (category-id shop-id)
  (make-dao 'shop-2-category
            :shop-id shop-id
            :category-id category-id)
  t)

;;  API : PRODUCT

(defun create-product (category-id)
  (id (make-dao 'product :category-id category-id)))

(defun product-add-option (product-id lang name &optional value)
  (let ((product    (get-dao   'product product-id))
        (lang-id     (query     (:select 'id :from 'lang :where (:= 'val lang)) :single)))
    (let ((option-id (id (make-dao  'option :optype "product-option"))))
      (make-dao 'optname
                :option-id option-id
                :lang-id lang-id
                :val name)
      (when value
        (make-dao 'optvalue
                  :option-id option-id
                  :lang-id lang-id
                  :val value))
      t)))


(let ((shop-id (create-shop)))
  (shop-add-option shop-id "ru" "Имя организации" "Макарена")
  (let ((category-id (create-category)))
    (category-add-option category-id "ru" "Имя категории" "Холодные закуски")
    (addlink-category-2-shop shop-id category-id)
    (let ((product-id (create-product category-id)))
      (product-add-option product-id "ru" "Имя продукта" "Бутерброд"))))


;; ;; example data access
;; (remove-if #'null
;;            (loop
;;               :for item
;;               :in (query (sql (:select '* :from 'option :where (:= 'product-id 1))))
;;               :collect (destructuring-bind (product-id optname-id optvalue-id)
;;                            item
;;                          (query (sql (:select 'id 'lang-id 'val :from 'optname :where (:and
;;                                                                                        (:= 'id optname-id)
;;                                                                                        (:= 'lang-id 2))))))))

