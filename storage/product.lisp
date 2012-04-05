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


(connect-toplevel "restodb" "resto" "resto1111" "localhost")

;; produce (and init if need) linktable object
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
(defmacro incrementor (name)
  `(let ((,(intern (format nil "INC-~A-ID" (symbol-name name))) 0))
     (list
      (defun ,(intern (format nil "INCF-~A-ID" (symbol-name name) ())) ()
        (incf ,(intern (format nil "INC-~A-ID" (symbol-name name)))))
      (defun ,(intern (format nil "INIT-~A-ID" (symbol-name name) ())) (init-value)
        (setf ,(intern (format nil "INC-~A-ID" (symbol-name name))) init-value)))))


;; incrementor test
(print (macroexpand-1 '(incrementor product)))
;; (LET ((INC-PRODUCT-ID))
;;   (LIST
;;     (DEFUN INCF-PRODUCT-ID ()
;;       (INCF INC-PRODUCT-ID))
;;     (DEFUN INIT-PRODUCT-ID (INIT-VALUE)
;;       (SETF INC-PRODUCT-ID INIT-VALUE))))


;;  LANG

(defclass lang ()
  ((id                :col-type integer         :initarg :id             :initform (incf-lang-id) :accessor id)
   (val               :col-type string          :initarg :val            :initform ""         :accessor val))
  (:metaclass dao-class)
  (:keys id))

(defun init-lang ()
  (query (sql (:drop-table :if-exists 'lang)))
  (incrementor lang)
  (execute (dao-table-definition 'lang))
  (make-dao 'lang :val "ru")
  (make-dao 'lang :val "en"))

(init-lang)


;;  OPTNAME

(defclass optname ()
  ((option-id         :col-type integer         :initarg :option-id      :initform 0          :accessor option-id)
   (lang-id           :col-type integer         :initarg :lang-id        :initform 0          :accessor lang-id)
   (val               :col-type string          :initarg :val            :initform ""         :accessor val))
  (:metaclass dao-class))

(defun init-optname ()
  (query (sql (:drop-table :if-exists 'optname)))
  (execute (dao-table-definition 'optname)))

(init-optname)


;;  OPTVALUE

(defclass optvalue ()
  ((option-id         :col-type integer         :initarg :option-id      :initform 0          :accessor option-id)
   (lang-id           :col-type integer         :initarg :lang-id        :initform 0          :accessor lang-id)
   (val               :col-type string          :initarg :val            :initform ""         :accessor val)
   (product-id        :col-type integer         :initarg :product-id     :initform 0          :accessor product-id))
  (:metaclass dao-class))

(defun init-optvalue ()
  (query (sql (:drop-table :if-exists 'optvalue)))
  (execute (dao-table-definition 'optvalue)))

(init-optvalue)

;;  OPTION

(defclass option ()
  ((id                :col-type integer         :initarg :id             :initform (incf-option-id)  :accessor id)
   (parent-id         :col-type integer         :initarg :parent-id      :initform 0          :accessor parent-id)
   (optype            :col-type string          :initarg :optype         :initform ""         :accessor optype))
  (:metaclass dao-class)
  (:keys id))

(defun init-option ()
  (query (sql (:drop-table :if-exists 'option)))
  (incrementor option)
  (execute (dao-table-definition 'option)))

(init-option)


;;  PRODUCT

;; class product
(defclass product ()
  ((id                :col-type integer         :initarg :id              :initform (incf-product-id) :accessor id)
   (category-id       :col-type integer         :initarg :category-id     :initform 0         :accessor category-id)
   (options                                     :initarg :opions          :initform ""        :accessor options))
  (:metaclass dao-class)
  (:keys id))


(defun init-product ()
  (query (sql (:drop-table :if-exists 'product)))
  (incrementor product)
  (execute (dao-table-definition 'product)))

(init-product)


;;  PRODUCT-2-OPTION

(def~daoclass-linktable product option t)



;;  CATEGORY

(defclass category ()
  ((id                :col-type integer         :initarg :id              :initform (incf-category-id) :accessor id)
   (parent-id         :col-type integer         :initarg :parent-id       :initform 0         :accessor parent-id)
   (shop-id           :col-type integer         :initarg :shop-id         :initform 0         :accessor shop-id))
  (:metaclass dao-class)
  (:keys id))


(defun init-category ()
  (query (sql (:drop-table :if-exists 'category)))
  (incrementor category)
  (execute (dao-table-definition 'category)))

(init-category)


;;  CATEGORY-2-OPTION

(def~daoclass-linktable category option t)


;;  SHOP

(defclass shop ()
  ((id                :col-type integer         :initarg :id              :initform (incf-shop-id) :accessor id))
  (:metaclass dao-class)
  (:keys id))

(defun init-shop ()
  (query (sql (:drop-table :if-exists 'shop)))
  (incrementor shop)
  (execute (dao-table-definition 'shop)))

(init-shop)

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

