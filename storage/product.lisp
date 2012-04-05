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

(incrementor lang)

(defclass lang ()
  ((id                :col-type integer         :initarg :id             :initform (incf-lang-id) :accessor id)
   (val               :col-type string          :initarg :val            :initform ""         :accessor val))
  (:metaclass dao-class)
  (:keys id))

(execute (dao-table-definition 'lang))


;; === OPTNAME ===

(incrementor optname)

(defclass optname ()
  ((id                :col-type integer         :initarg :id             :initform (incf-optname-id) :accessor id)
   (lang-id           :col-type integer         :initarg :lang-id        :initform 0          :accessor lang-id)
   (val               :col-type string          :initarg :val            :initform ""         :accessor val))
  (:metaclass dao-class)
  (:keys id))

(execute (dao-table-definition 'optname))


;; === OPTVALUE ===

(incrementor optvalue)

(defclass optvalue ()
  ((id                :col-type integer         :initarg :id             :initform (incf-optvalue-id) :accessor id)
   (lang-id           :col-type integer         :initarg :lang-id        :initform 0          :accessor lang-id)
   (val               :col-type string          :initarg :val            :initform ""         :accessor val))
  (:metaclass dao-class)
  (:keys id))

(execute (dao-table-definition 'optvalue))


;; === OPTION ===

(defclass option ()
  ((product-id        :col-type integer         :initarg :product-id     :initform 0          :accessor product-id)
   (optname-id        :col-type integer         :initarg :optname-id     :initform 0          :accessor optname-id)
   (optvalue-id       :col-type integer         :initarg :optvalue-id    :initform 0          :accessor optvalue-id))
  (:metaclass dao-class)
  (:keys product-id optname-id optvalue-id))

(execute (dao-table-definition 'option))


;; === PRODUCT ===

(incrementor product)

;; class product
(defclass product ()
  ((id                :col-type integer         :initarg :id              :initform (incf-product-id)  :accessor id)
   (price             :col-type integer         :initarg :price           :initform ""        :accessor price)
   (opts                                        :initarg :opts            :initform ""        :accessor opts))
  (:metaclass dao-class)
  (:keys id))

(execute (dao-table-definition 'product))


;; === TESTS ===


(defun db-init ()
  ;; drop
  (query (sql (:drop-table :if-exists 'lang)))
  (query (sql (:drop-table :if-exists 'optname)))
  (query (sql (:drop-table :if-exists 'optvalue)))
  (query (sql (:drop-table :if-exists 'option)))
  (query (sql (:drop-table :if-exists 'product)))
  ;; create tables
  (execute (dao-table-definition 'lang))
  (execute (dao-table-definition 'optname))
  (execute (dao-table-definition 'optvalue))
  (execute (dao-table-definition 'option))
  (execute (dao-table-definition 'product))
  ;; incrementors
  (incrementor lang)
  (incrementor optname)
  (incrementor optvalue)
  (incrementor product)
  ;; clear tables
  (query (:delete-from 'product))
  (query (:delete-from 'option))
  (query (:delete-from 'optname))
  (query (:delete-from 'optvalue))
  (query (:delete-from 'lang))
  ;; lang
  (make-dao 'lang :val "ru")
  (make-dao 'lang :val "en")
  ;; optname
  (make-dao 'optname :lang-id 1 :val "рус опт 1 имя")
  (make-dao 'optname :lang-id 1 :val "рус опт 2 имя")
  (make-dao 'optname :lang-id 2 :val "eng opt 1 name")
  (make-dao 'optname :lang-id 2 :val "eng opt 1 name")
  ;; optvalue
  (make-dao 'optvalue :lang-id 1 :val "рус опт 1 значение")
  (make-dao 'optvalue :lang-id 1 :val "рус опт 2 значение")
  (make-dao 'optvalue :lang-id 2 :val "eng opt 1 value")
  (make-dao 'optvalue :lang-id 2 :val "eng opt 1 value")
  ;; option
  (make-dao 'option :product-id 1 :optname-id 1 :optvalue-id 1)
  (make-dao 'option :product-id 1 :optname-id 2 :optvalue-id 2)
  (make-dao 'option :product-id 1 :optname-id 3 :optvalue-id 3)
  (make-dao 'option :product-id 1 :optname-id 4 :optvalue-id 4)

  ;; product
  (make-dao 'product :price (random 700))
  )


(db-init)


(remove-if #'null
           (loop
              :for item
              :in (query (sql (:select '* :from 'option :where (:= 'product-id 1))))
              :collect (destructuring-bind (product-id optname-id optvalue-id)
                           item
                         (query (sql (:select 'id 'lang-id 'val :from 'optname :where (:and
                                                                                       (:= 'id optname-id)
                                                                                       (:= 'lang-id 2))))))))





;; insert product
(insert-dao
 (make-instance 'product
  :name "Тестовый Продукт"
  :price (random 700)))

(query (:select '* :from 'product))
;; (query (:delete-from 'product))

;; update product
(let ((test-product (get-dao 'product 1)))
  (setf (price test-product) 4500000)
  (update-dao test-product))


;; === PRODUCT ===

(incrementor product)

;; class product
(defclass product ()
  ((id                :col-type integer         :initarg :id              :initform (incf-product-id)  :accessor id)
   (name              :col-type string          :initarg :name            :initform ""        :accessor name)
   (price             :col-type integer         :initarg :price           :initform ""        :accessor price)
   (opts                                        :initarg :opts            :initform ""        :accessor opts))
  (:metaclass dao-class)
  (:keys id))

(execute (dao-table-definition 'product))


;; === TESTS ===

;; insert product
(insert-dao
 (make-instance 'product
  :name "Тестовый Продукт"
  :price (random 700)))

(query (:select '* :from 'product))
;; (query (:delete-from 'product))

;; update product
(let ((test-product (get-dao 'product 1)))
  (setf (price test-product) 4500000)
  (update-dao test-product))


;; Prepared Statements
(defprepared price-of-product
    (:select 'price :from 'product :where (:= 'name '$1))
  :single!)

(price-of-product "Тестовый Продукт")


;; === HASH-TABLES ===
;; Это пример, который иллюстрирует что будет, если мы будем кешировать в хэш-таблицах продукты
;; Он довольно синтетический, но дает понимание зачем тут использовать макроc create-product

(defparameter *h-product* (make-hash-table))

(defmacro create-product (&body params)
  `(let ((id (incf-product-id)))
     (values
      (setf (gethash id *h-product*)
            (make-dao
             'product
             :id id
             ,@params))
      id)))

(print (macroexpand-1 '(create-product :name "Еще продукт" :price (random 700))))
;; (LET ((ID (INCF-PRODUCT-ID)))
;;   (VALUES
;;    (SETF (GETHASH ID *H-PRODUCT*)
;;          (MAKE-DAO 'PRODUCT :ID ID :NAME "Еще продукт" :PRICE (RANDOM 700)))
;;    ID))

(create-product :name "Еще продукт" :price (random 700))


(make-dao 'option
          :product-id 3
          :name "опт-name2"
          :value "опт-value2")

(query (:select '* :from 'option))


;; (add-option-to-product-by-id 5 "оптнаме2" "оптвалуе2")



;; get options
(query (sql (:select '* :from 'product
                     :inner-join 'option :on (:= 'product.id 'option.product-id)
                     :where (:= 'product.id 3))))


;; (defun add-option (pr-name &rest opt-list)
;;   "opt-list должен быть plist-ом"
;;   (unless (and opt-list
;;                (evenp (length opt-list)))
;;     (format t "неправильный opt-list")
;;     (return-from add-option))
;;   (let ((prod-list (car (select-dao 'product (:ilike 'name pr-name)))))
;;     (when (null prod-list)
;;       (setf prod-list (add-product pr-name)))
;;     (loop :for i :from 0 :upto (1- (length opt-list)) :by 2
;;        :do (let* ((op-key (nth i opt-list))
;;                   (op-val (nth (1+ i) opt-list))
;;                   (key-list (car (select-dao 'opname (:ilike 'name op-key)))))
;;              (when (null key-list)
;;                (setf key-list (add-opname op-key)))
;;              (make-dao 'option
;;                        :pr-id (product-id prod-list)
;;                        :op-id (opnamet-id key-list)
;;                        :value op-val)))))


;; (execute (dao-table-definition 'product))
;; (execute (dao-table-definition 'opname))
;; (execute (dao-table-definition 'option))
;; (query "ALTER TABLE product ADD CONSTRAINT uq_p_name UNIQUE (name)")
;; (query "ALTER TABLE product ADD CHECK (name <> '')")
;; (query "ALTER TABLE opname ADD CONSTRAINT uq_o_name UNIQUE (name)")
;; (query "ALTER TABLE opname ADD CHECK (name <> '')")
;; (query "ALTER TABLE option ADD FOREIGN KEY (pr_id) REFERENCES product(id)")
;; (query "ALTER TABLE option ADD FOREIGN KEY (op_id) REFERENCES opname(id)")
;; (add-product "oil")

;; (add-product "масло A")
;; (add-product "масло B")
;; (add-opname "объём")
;; (add-opname "вязкость")

;; (add-option "oil" "объём" "3л." "объём" "4л.")
;; (add-option "oil" "объём" "3,5л." "объём" "4,5л.")
;; (add-option "масло U" "вес" "7,5г")
;; (add-option "масло U" "вес" "7,5г" "gjh")
;; (add-option "oil" "объём" "3,5л." "объём" "4,5л.")
