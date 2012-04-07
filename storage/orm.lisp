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
;; (connect-toplevel *db-name* *db-user* *db-pass* *db-serv*)

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
;; (print (macroexpand-1 '(incrementor product id)))
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
                                          'option-id option-id))))
                 (defmethod get-opts ((dao-obj ,name) lang)
                   (let* ((lang-id    (query (:select 'id :from 'lang :where (:= 'code lang)) :single))
                          (option-ids (mapcar #'car
                                              (query (:select 'option.id :from ',(intern (format nil "~A-2-OPTION" (symbol-name name)))
                                                              :inner-join 'option
                                                              :on (:= ',(intern (format nil "~A-2-OPTION.OPTION-ID" (symbol-name name)))
                                                                      'option.id)
                                                              :where (:= ',(intern (format nil "~A-ID" name))
                                                                         (id dao-obj)))))))
                     (loop :for item :in option-ids
                        :when (let ((name   (query (:select 'val :from 'optname  :where (:and (:= 'lang-id lang-id) (:= 'option-id item))) :single))
                                    (value  (query (:select 'val :from 'optvalue :where (:and (:= 'lang-id lang-id) (:= 'option-id item))) :single)))
                                (if name    (cons name value) nil))
                        :collect it)))
                 (defmethod get-option ((dao-obj ,name) lang name)
                   (cdr (assoc name (get-opts dao-obj lang) :test #'equal))))))))


;; entity test
;; (print (macroexpand-1 '(def~daoclass-entity product ()
;;                         ((id                :col-type integer         :initform (incf-product-id))
;;                          (category-id       :col-type integer         :initform 0)
;;                          (options                                     :initform ""))
;;                         (:keys id)
;;                         (:incf id)
;;                         (:re-init t)
;;                         (:re-link t))))

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
;;                              'OPTION-ID OPTION-ID))))
;;     (DEFMETHOD GET-OPTS ((DAO-OBJ PRODUCT) LANG)
;;       (LET* ((LANG-ID     (QUERY (:SELECT 'ID :FROM 'LANG :WHERE (:= 'CODE LANG)) :SINGLE))
;;              (OPTION-IDS  (MAPCAR #'CAR
;;                                   (QUERY
;;                                    (:SELECT 'OPTION.ID :FROM 'PRODUCT-2-OPTION
;;                                             :INNER-JOIN 'OPTION :ON (:= 'PRODUCT-2-OPTION.OPTION-ID 'OPTION.ID)
;;                                             :WHERE (:= 'PRODUCT-ID (ID DAO-OBJ)))))))
;;         (LOOP :FOR ITEM :IN OPTION-IDS
;;            :WHEN (LET ((NAME   (QUERY (:SELECT 'VAL :FROM 'OPTNAME  :WHERE (:AND (:= 'LANG-ID LANG-ID) (:= 'OPTION-ID ITEM))) :SINGLE))
;;                        (VALUE  (QUERY (:SELECT 'VAL :FROM 'OPTVALUE :WHERE (:AND (:= 'LANG-ID LANG-ID) (:= 'OPTION-ID ITEM))) :SINGLE)))
;;                    (IF NAME (CONS NAME VALUE) NIL))
;;            :COLLECT IT)))
;;     (DEFMETHOD GET-OPTION ((DAO-OBJ PRODUCT) LANG NAME)
;;       (CDR (ASSOC NAME (GET-OPTS DAO-OBJ LANG) :TEST #'EQUAL)))))


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


;; write to lang & lang_2_option
(defparameter *ru* (make-dao 'lang :code "ru"))
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
(defparameter *narv* (make-dao 'city :country-id (id *spb*) :city-code (code *spb*) :code "narv"))
(add-option *narv* "ru" "name" "Нарвская")
(add-option *narv* "en" "name" "Narvskaya")
