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


(let ((inc-product-id 0))
  (defun inc-product-id ()
    (incf inc-product-id)))


(defclass product ()
  ((id                :col-type integer         :initarg :id              :initform 0         :accessor id)
   (name              :col-type string          :initarg :name            :initform ""        :accessor name)
   (price             :col-type integer         :initarg :price           :initform ""        :accessor price)
   (opts                                        :initarg :opts            :initform ""        :accessor opts))
  (:metaclass dao-class)
  (:keys id))

;; (execute (dao-table-definition 'product))


(insert-dao
 (make-instance
  'product
  :id (inc-product-id)
  :name "Тестовый Продукт"
  :price 200
  :opts nil))

(query (:select '* :from 'product))


(let ((test-product (get-dao 'product 4)))
  (setf (price test-product) 4500000)
  (update-dao test-product))

(query (:select '* :from 'country_table))

(dao-keys 'product)

(dao-keys (get-dao 'product 4))

(sql (:select 'relname :from 'pg-catalog.pg-class
              :inner-join 'pg-catalog.pg-namespace :on (:= 'relnamespace 'pg-namespace.oid)
              :where (:and (:= 'relkind "r")
                           (:not-in 'nspname (:set "pg_catalog" "pg_toast"))
                           (:pg-catalog.pg-table-is-visible 'pg-class.oid))))

;; Prepared Statements
(defprepared sovereign-of
    (:select 'sovereign :from 'country_table :where (:= 'name '$1))
  :single!)

(sovereign-of "The Netherlands")


(make-dao 'country :name "Германия" :inhabitants 80000000 :sovereign "Меркель")

(query "select * from country_table")
;;(("The Netherlands" 16400000 "Beatrix") ("Croatia" 4500000 :NULL)
;; ("Германия" 80000000 "Меркель"))

(query (:select '* :from '#:country-table))
;;(("The Netherlands" 16400000 "Beatrix") ("Croatia" 4500000 :NULL)
;;  ("Германия" 80000000 "Меркель"))
;;3

(query (:select '* :from '#:country-table) :alist)
;;((:NAME . "The Netherlands") (:INHABITANTS . 16400000) (:SOVEREIGN . "Beatrix"))
;;3

(query (:select '* :from '#:country-table) :alists)
;; (((:NAME . "The Netherlands") (:INHABITANTS . 16400000)
;;     (:SOVEREIGN . "Beatrix"))
;;   ((:NAME . "Croatia") (:INHABITANTS . 4500000) (:SOVEREIGN . :NULL))
;;   ((:NAME . "Германия") (:INHABITANTS . 80000000) (:SOVEREIGN . "Меркель")))
;; 3


(defparameter *prod-id* 0 "Переменная автоинкремента для product")

(defun *prod-id* ()
  "Автоинкремент для product"
  (incf *prod-id*))

(defclass product ()
  ((id :col-type integer :initarg :id :accessor product-id)
   (name :col-type string :initarg :name :accessor product-name))
  (:metaclass dao-class)
  (:keys id))

(defun add-product (name)
    "Проверка на дубликат в БД"
    (make-dao 'product
              :id (*prod-id*)
              :name name))

(defparameter *opname-id* 0
    "Переменная автоинкремента для opname")

(defun *opname-id* ()
    "Автоинкремент для opname"
      (incf *opname-id*))

(defclass opname ()
    ((id :col-type integer :initarg :id :accessor opnamet-id)
        (name :col-type string :initarg :name :accessor opname-name))
    (:metaclass dao-class)
      (:keys id))

(defun add-opname (name)
    "Проверка на дубликат в БД"
    (make-dao 'opname
              :id (*opname-id*)
              :name name))

(defparameter *option-id* 0
  "Переменная автоинкремента для option")

(defun *option-id* ()
  "Автоинкремент для option"
  (incf *option-id*))

(defclass option ()
  ((pr-id :col-type integer :initarg :pr-id :accessor option-pr-id)
   (op-id :col-type integer :initarg :op-id :accessor option-op-id)
   (value :col-type string :initarg :value :accessor option-value))
  (:metaclass dao-class)
  (:keys pr-id op-id value))

(defun add-option (pr-name &rest opt-list)
  "opt-list должен быть plist-ом"
  (unless (and opt-list
               (evenp (length opt-list)))
    (format t "неправильный opt-list")
    (return-from add-option))
  (let ((prod-list (car (select-dao 'product (:ilike 'name pr-name)))))
    (when (null prod-list)
      (setf prod-list (add-product pr-name)))
    (loop :for i :from 0 :upto (1- (length opt-list)) :by 2
       :do (let* ((op-key (nth i opt-list))
                  (op-val (nth (1+ i) opt-list))
                  (key-list (car (select-dao 'opname (:ilike 'name op-key)))))
             (when (null key-list)
               (setf key-list (add-opname op-key)))
             (make-dao 'option
                       :pr-id (product-id prod-list)
                       :op-id (opnamet-id key-list)
                       :value op-val)))))


(execute (dao-table-definition 'product))

(execute (dao-table-definition 'opname))

(execute (dao-table-definition 'option))

(query "ALTER TABLE product ADD CONSTRAINT uq_p_name UNIQUE (name)")

(query "ALTER TABLE product ADD CHECK (name <> '')")

(query "ALTER TABLE opname ADD CONSTRAINT uq_o_name UNIQUE (name)")

(query "ALTER TABLE opname ADD CHECK (name <> '')")

(query "ALTER TABLE option ADD FOREIGN KEY (pr_id) REFERENCES product(id)")

(query "ALTER TABLE option ADD FOREIGN KEY (op_id) REFERENCES opname(id)")

(add-product "oil")

(add-product "масло A")
(add-product "масло B")
(add-opname "объём")
(add-opname "вязкость")

(add-option "oil" "объём" "3л." "объём" "4л.")

(add-option "oil" "объём" "3,5л." "объём" "4,5л.")

(add-option "масло U" "вес" "7,5г")

(add-option "масло U" "вес" "7,5г" "gjh")

(add-option "oil" "объём" "3,5л." "объём" "4,5л.")
