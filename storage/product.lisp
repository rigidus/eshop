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



;; === LANG ===

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


;; === OPTNAME ===

(defclass optname ()
  ((option-id         :col-type integer         :initarg :option-id      :initform 0          :accessor option-id)
   (lang-id           :col-type integer         :initarg :lang-id        :initform 0          :accessor lang-id)
   (val               :col-type string          :initarg :val            :initform ""         :accessor val))
  (:metaclass dao-class))

(defun init-optname ()
  (query (sql (:drop-table :if-exists 'optname)))
  (execute (dao-table-definition 'optname))
  (make-dao 'optname :val "имя-опции-1" :lang-id (query (:select 'id :from 'lang :where (:= 'val "ru")) :single))
  (make-dao 'optname :val "option-name-1"  :lang-id (query (:select 'id :from 'lang :where (:= 'val "en")) :single))
  (make-dao 'optname :val "имя-опции-2" :lang-id (query (:select 'id :from 'lang :where (:= 'val "ru")) :single))
  (make-dao 'optname :val "option-name-2"  :lang-id (query (:select 'id :from 'lang :where (:= 'val "en")) :single)))

(init-optname)


;; === OPTVALUE ===

(defclass optvalue ()
  ((option-id         :col-type integer         :initarg :option-id      :initform 0          :accessor option-id)
   (lang-id           :col-type integer         :initarg :lang-id        :initform 0          :accessor lang-id)
   (val               :col-type string          :initarg :val            :initform ""         :accessor val)
   (product-id        :col-type integer         :initarg :product-id     :initform 0          :accessor product-id))
  (:metaclass dao-class))

(defun init-optvalue ()
  (query (sql (:drop-table :if-exists 'optvalue)))
  (execute (dao-table-definition 'optvalue))
  (make-dao 'optvalue :val "значение-опции-1" :lang-id (query (:select 'id :from 'lang :where (:= 'val "ru")) :single))
  (make-dao 'optvalue :val "option-value-1"  :lang-id (query (:select 'id :from 'lang :where (:= 'val "en")) :single))
  (make-dao 'optvalue :val "значение-опции-2" :lang-id (query (:select 'id :from 'lang :where (:= 'val "ru")) :single))
  (make-dao 'optvalue :val "option-value-2"  :lang-id (query (:select 'id :from 'lang :where (:= 'val "en")) :single)))

(init-optvalue)

;; === OPTION ===

(defclass option ()
  ((id                :col-type integer         :initarg :id             :initform (incf-option-id)  :accessor id)
   (parent-id         :col-type integer         :initarg :parent-id      :initform 0          :accessor parent-id)
   (optype            :col-type string          :initarg :optype         :initform ""         :accessor optype))
  (:metaclass dao-class)
  (:keys id))

(defun init-option ()
  (query (sql (:drop-table :if-exists 'option)))
  (incrementor option)
  (execute (dao-table-definition 'option))
  (let ((option-id (id (make-dao 'option :optype "option"))))
    (query (:update 'optname :set 'option-id option-id :where (:= 'val "имя-опции-1")))
    (query (:update 'optname :set 'option-id option-id :where (:= 'val "option-name-1")))
    (query (:update 'optvalue :set 'option-id option-id :where (:= 'val "значение-опции-1")))
    (query (:update 'optvalue :set 'option-id option-id :where (:= 'val "option-value-1"))))
  (let ((option-id (id (make-dao 'option :optype "option"))))
    (query (:update 'optname :set 'option-id option-id :where (:= 'val "имя-опции-2")))
    (query (:update 'optname :set 'option-id option-id :where (:= 'val "option-name-2")))
    (query (:update 'optvalue :set 'option-id option-id :where (:= 'val "значение-опции-2")))
    (query (:update 'optvalue :set 'option-id option-id :where (:= 'val "option-value-2")))))

(init-option)


;; === PRODUCT ===

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
  (execute (dao-table-definition 'product))
  (make-dao 'product))

(init-product)


;; === PRODUCT-2-OPTION

(defclass product-2-option ()
  ((product-id        :col-type integer         :initarg :product-id      :initform 0         :accessor product-id)
   (option-id         :col-type integer         :initarg :option-id       :initform 0         :accessor option-id))
  (:metaclass dao-class))

(defun init-product-2-option ()
  (query (sql (:drop-table :if-exists 'product-2-option)))
  (execute (dao-table-definition 'product-2-option))
  (make-dao 'product-2-option :product-id 1 :option-id 1)
  (make-dao 'product-2-option :product-id 1 :option-id 2))

(init-product-2-option)


;; === CATEGORY ===

(defclass category ()
  ((id                :col-type integer         :initarg :id              :initform (incf-category-id) :accessor id)
   (parent-id         :col-type integer         :initarg :parent-id       :initform 0         :accessor parent-id))
  (:metaclass dao-class)
  (:keys id))


(defun init-category ()
  (query (sql (:drop-table :if-exists 'category)))
  (incrementor category)
  (execute (dao-table-definition 'category))
  (make-dao 'category))

(init-category)


;; === CATEGORY-2-OPTION

(defclass category-2-option ()
  ((category-id       :col-type integer         :initarg :category-id     :initform 0         :accessor category-id)
   (option-id         :col-type integer         :initarg :option-id       :initform 0         :accessor option-id))
  (:metaclass dao-class))

(defun init-category-2-option ()
  (query (sql (:drop-table :if-exists 'category-2-option)))
  (execute (dao-table-definition 'category-2-option))
  (make-dao 'category-2-option :product-id 1 :option-id 1)
  (make-dao 'category-2-option :product-id 1 :option-id 2))

(init-category-2-option)


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

