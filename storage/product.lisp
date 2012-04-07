;;;; product.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Authors:
;;;;   Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>
;;;;   Fedorov Evgeniy aka Zhef

(in-package #:eshop.storage)








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

