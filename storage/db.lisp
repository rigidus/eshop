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
;; (disconnect-toplevel)


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
                 (defmethod make-option ((dao-obj ,name) lang name &key (optype "") (parent-id 0))
                   (let* ((lang-id    (get-lang-id lang))
                          (option     (make-dao 'option :lang-id lang-id :name name
                                                :optype optype :parent-id parent-id)))
                     (query (:insert-into ',(intern (format nil "~A-2-OPTION" (symbol-name name))) :set
                                          ',(intern (format nil "~A-ID" (symbol-name name))) (id dao-obj)
                                          'option-id (id option)))
                     option))
                 (defmethod load-options ((dao-obj ,name) &key lang name optype parent-id)
                   (let ((rs (loop
                                :for item
                                :in (mapcar #'car
                                            (query (:select 'option-id
                                                            :from  ',(intern (format nil "~A-2-OPTION" (symbol-name name)))
                                                            :where (:= ',(intern (format nil "~A-ID" (symbol-name name))) (id dao-obj)))))
                                :collect (initialize-instance (get-dao 'option item)))))
                     (when lang (setf rs (remove-if-not #'(lambda (x) (equal (lang-id x) (get-lang-id lang))) rs)))
                     (when name (setf rs (remove-if-not #'(lambda (x) (equal name (name x))) rs)))
                     (when optype (setf rs (remove-if-not #'(lambda (x) (equal optype (optype x))) rs)))
                     (when parent-id (setf rs (remove-if-not #'(lambda (x) (equal parent-id (parent-id x))) rs)))
                     rs))
                 (defmethod get-opts-val ((dao-obj ,name))
                   (mapcar #'(lambda (option)
                               (list
                                (name option)
                                (mapcar #'(lambda (optval)
                                            (val optval))
                                        (load-values option))))
                           (load-options dao-obj))))))))


;; entity test
;; (print (macroexpand-1 '(def~daoclass-entity product ()
;;                         ((id                :col-type integer         :initform (incf-product-id))
;;                          (category-id       :col-type integer         :initform 0)
;;                          (options                                     :initform ""))
;;                         (:keys id)
;;                         (:incf id)
;;                         (:re-init t)
;;                         (:re-link t))))



(defmacro get-lang-id (lang)
  `(if (integerp ,lang)
       lang
       (aif (query (:select 'id :from 'lang :where (:= 'code ,lang)) :single) it (error 'lang-not-found))))

;; (macroexpand-1 '(get-lang-id "ru"))
;; (get-lang-id "ru")


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

(defmethod make-value ((option-obj option) lang val)
  (make-dao 'optval
            :option-id (id option-obj)
            :lang-id (get-lang-id lang)
            :val val))

(defmethod load-values ((option-obj option) &key lang)
  (loop
     :for item
     :in (aif lang
              (query-dao 'optval (:select '* :from 'optval :where (:and (:= 'lang-id (get-lang-id lang)) (:= 'option-id (id option-obj)))))
              (query-dao 'optval (:select '* :from 'optval :where (:= 'option-id (id option-obj)))))
     :collect (initialize-instance item)))


;; COUNTRY
(def~daoclass-entity country ()
  ((id                :col-type integer         :initform (incf-country-id))
   (code              :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t)
  (:re-link t))


(defun get-all-opt-val (dao-obj &key (optname-func #'identity) (optvalue-func #'identity))
  (loop :for item :in (load-options dao-obj) :collect
     (list (funcall optname-func item) (mapcar optvalue-func (load-values item)))))

(defun get-all-entityes-opt-val (list-of-entityes &key (entity-func #'identity) (optname-func #'identity) (optvalue-func #'identity))
  (loop :for item :in list-of-entityes :collect
     (list (funcall entity-func item)
           (get-all-opt-val item :optname-func optname-func :optvalue-func optvalue-func))))


;; (get-all-entityes-opt-val (select-dao 'country)
;;                           :entity-func #'(lambda (country) (list (id country) (code country)))
;;                           :optname-func #'(lambda (option) (name option))
;;                           :optvalue-func #'(lambda (optval) (val optval)))

;; (get-all-entityes-opt-val (select-dao 'country))


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


;; special macro for 0 dao-obj (unlinked option)
(defmethod make-option ((zero-obj (eql 0)) lang name &key (optype "") (parent-id 0))
  (let* ((lang-id    (get-lang-id lang)))
    (make-dao 'option :lang-id lang-id :name name :optype optype :parent-id parent-id)))


(defmethod load-child-options ((dao-option option))
  (select-dao 'option (:= 'parent-id (id dao-option))))


(defmethod opt-tree ((param option) &key (lang "ru"))
  (let ((childs (load-child-options param)))
    (if childs
        (list (aif (car (load-values param :lang lang))
                   (val it)
                   "Нет названия")
              (mapcar #'ru-vals childs))
        (list (aif (car (load-values param :lang lang))
                   (val it)
                   "Нет названия")))))


;;  SHOP

(def~daoclass-entity shop ()
  ((id                :col-type integer         :initform (incf-shop-id))
   (code              :col-type string          :initform "")
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
   (city-id           :col-type integer         :initform 0)
   (city-code         :col-type string          :initform "")
   (rating            :col-type float           :initform 0.0)
   (rating-count      :col-type integer         :initform 0)
   (comment-count     :col-type integer         :initform 0)
   (worktime          :col-type string          :initform ""))
  ;; + name description phone subways street building
  (:keys id)
  (:incf id)
  (:re-init t)
  (:re-link t))

(def~daoclass-linktable shop subway t)


;; CATEGORY

(def~daoclass-entity category ()
  ((id                :col-type integer         :initform (incf-category-id))
   (parent-id         :col-type integer         :initform 0)
   (image             :col-type string          :initform "")
   (code              :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t)
  (:re-link t))

(def~daoclass-linktable shop category t)



;; PRODUCT

(def~daoclass-entity product ()
  ((id                :col-type integer         :initform (incf-product-id))
   (code              :col-type string          :initform "")
   (price             :col-type float           :initform 0.0)
   (photo-small       :col-type string          :initform "")
   (photo-big         :col-type string          :initform "")
   (delivery          :col-type integer         :initform 0)
   (rating            :col-type float           :initform 0.0)
   (rating_count      :col-type integer         :initform 0)
   (comment_count     :col-type integer         :initform 0))
  (:keys id)
  (:incf id)
  (:re-init t)
  (:re-link t))

(def~daoclass-linktable shop product t)
(def~daoclass-linktable category product t)


(defmacro mv (lang val)
  `(make-value opt ,lang ,val))

(defmacro mo (dao lang name parent-id &body body)
  `(let* ((opt (make-option ,dao ,lang ,name :parent-id ,parent-id))
          (po  (id opt)))
     ,@(loop :for item :in body :collect item)
     opt))

