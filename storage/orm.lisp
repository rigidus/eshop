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


(defmacro get-lang-id (lang)
  `(if (integerp ,lang)
       lang
       (aif (query (:select 'id :from 'lang :where (:= 'code ,lang)) :single) it (error 'lang-not-found))))

;; (macroexpand-1 '(get-lang-id "ru"))
;; (get-lang-id "ru")


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
  (loop :for item :in (aif lang
                           (query-dao 'optval (:select '* :from 'optval :where (:and (:= 'lang-id (get-lang-id lang)) (:= 'option-id (id option-obj)))))
                           (query-dao 'optval (:select '* :from 'optval :where (:= 'option-id (id option-obj)))))
     :collect (initialize-instance item)))


;; write to lang & lang_2_option
(progn
  (defparameter *ru* (make-dao 'lang :code "ru"))
  (make-option *ru* "ru" "Русский")
  (defparameter *en* (make-dao 'lang :code "en"))
  (make-option *en* "en" "English")
  (make-option *ru* "en" "Russian")
  (make-option *en* "ru" "Английский")
  (defparameter *it* (make-dao 'lang :code "it"))
  (make-option *it* "it" "Italiano")
  (make-option *it* "en" "Italian")
  (make-option *it* "ru" "Итальянский")
  (make-option *ru* "it" "Russo")
  (make-option *en* "it" "Inglese"))


;; (encode-json-to-string (load-options *en* :lang "en"))
;; (load-options *ru*)


;; COUNTRY

(def~daoclass-entity country ()
  ((id                :col-type integer         :initform (incf-country-id))
   (code              :col-type string          :initform ""))
  (:keys id)
  (:incf id)
  (:re-init t)
  (:re-link t))

(progn
  (defparameter *rus* (make-dao 'country :code "rus"))
  (let ((opt (make-option *rus* 0 "name")))
    (make-value opt "ru" "Россия")
    (make-value opt "en" "Russia")
    (make-value opt "it" "Rùssia")))

(defmacro mv (lang val)
  `(make-value opt ,lang ,val))

(progn
  (defparameter *usa* (make-dao 'country :code "usa"))
  (let ((opt (make-option *usa* 0 "name")))
    (mv "ru" "США")
    (mv "en" "USA")
    (mv "it" "Stati Uniti d'America")))

(defmacro mo (dao lang name parent-id &body body)
  `(let* ((opt (make-option ,dao ,lang ,name :parent-id ,parent-id))
          (po  (id opt)))
     ,@(loop :for item :in body :collect item)
     opt))

(progn
  (defparameter *ita* (make-dao 'country :code "ita"))
  (mo *ita* 0 "name" 0
    (mv "ru" "Италия")
    (mv "en" "Italy")
    (mv "it" "Itàlia")))

;; (val (car (load-values (car (load-options (car (select-dao 'country)) :name "name" :lang 0)) :lang "ru")))

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

(progn
  (defparameter *spb* (make-dao 'city :country-id (id *rus*) :country-code (code *rus*) :code "spb"))
  (mo *spb* 0 "name" 0
    (mv "ru" "Санкт-Петербург")
    (mv "en" "St.Peterburg")
    (mv "it" "San Pietroburgo"))
  (defparameter *mos* (make-dao 'city :country-id (id *rus*) :country-code (code *rus*) :code "mos"))
  (mo *mos* 0 "name" 0
    (mv "ru" "Москва")
    (mv "en" "Moscow")
    (mv "it" "Mosca"))
  (defparameter *nyk* (make-dao 'city :country-id (id *usa*) :country-code (code *usa*) :code "nyk"))
  (mo *nyk* 0 "name" 0
    (mv "ru" "Нью-Йорк")
    (mv "en" "New York")
    (mv "it" "New York"))
  (defparameter *rom* (make-dao 'city :country-id (id *ita*) :country-code (code *ita*) :code "rom"))
  (mo *rom* 0 "name" 0
    (mv "ru" "Рим")
    (mv "en" "Rome")
    (mv "it" "Róma")))

;; (print (get-all-entityes-opt-val (select-dao 'city)
;;                           :entity-func #'(lambda (city) (list (id city) (country-code city) (code city)))
;;                           :optname-func #'(lambda (option) (name option))
;;                           :optvalue-func #'(lambda (optval) (val optval))))


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

(progn
  (defparameter *avtovo* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "avtovo"))
  (mo *avtovo* 0 "name" 0
    (mv "ru" "Автово")
    (mv "en" "Avtovo"))
  (defparameter *narvskaya* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "narvskaya"))
  (mo *narvskaya* 0 "name" 0
    (mv "ru" "Нарвская")
    (mv "en" "Narvskaya"))
  (defparameter *kupchino* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "kupchino"))
  (mo *kupchino* 0 "name" 0
    (mv "ru" "Купчино")
    (mv "en" "Kupchino")
    (mv "it" "Kupchino"))
  (defparameter *zvezdnaya* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "zvezdnaya"))
  (mo *zvezdnaya* 0 "name" 0
    (mv "ru" "Звёздная")
    (mv "en" "Zvezdnaya")
    (mv "it" "Zvezdnaya"))
  (defparameter *moskovskaya* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "moskovskaya"))
  (mo *moskovskaya* 0 "name" 0
    (mv "ru" "Московская")
    (mv "en" "Moskovskaya")
    (mv "it" "Moskovskaya"))
  (defparameter *moskovskaya* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "moskovskaya"))
  (mo *moskovskaya* 0 "name" 0
    (mv "ru" "Московская")
    (mv "en" "Moskovskaya")
    (mv "it" "Moskovskaya"))
  (defparameter *park-pobedi* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "park-pobedi"))
  (mo *park-pobedi* 0 "name" 0
    (mv "ru" "Парк Победы")
    (mv "en" "Park Pobedi")
    (mv "it" "Park Pobedi"))
  (defparameter *electrosila* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "electrosila"))
  (mo *electrosila* 0 "name" 0
    (mv "ru" "Электросила")
    (mv "en" "Electrosila")
    (mv "it" "Electrosila"))
  (defparameter *moskovskie-vorota* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "moskovskie-vorota"))
  (mo *moskovskie-vorota* 0 "name" 0
    (mv "ru" "Московские ворота")
    (mv "en" "Moskovskie vorota")
    (mv "it" "Moskovskie vorota"))
  (defparameter *frunzenskaya* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "frunzenskaya"))
  (mo *frunzenskaya* 0 "name" 0
    (mv "ru" "Фрунзенская")
    (mv "en" "Frunzenskaya")
    (mv "it" "Frunzenskaya"))
  (defparameter *tehnologicheskiy-institut* (make-dao 'subway :city-id (id *spb*) :city-code (code *spb*) :code "tehnologicheskiy-institut"))
  (mo *tehnologicheskiy-institut* 0 "name" 0
    (mv "ru" "Технологический институт")
    (mv "en" "Tehnologicheskiy institut")
    (mv "it" "Tehnologicheskiy institut")))

;; (get-all-entityes-opt-val (select-dao 'subway)
;;                           :entity-func #'(lambda (subway) (list (id subway) (code subway)))
;;                           :optname-func #'(lambda (option) (name option))
;;                           :optvalue-func #'(lambda (optval) (val optval)))

;; PRE-CREATE OPTION

;; special macro for 0 dao-obj (unlinked option)
(defmethod make-option ((zero-obj (eql 0)) lang name &key (optype "") (parent-id 0))
  (let* ((lang-id    (get-lang-id lang)))
    (make-dao 'option :lang-id lang-id :name name :optype optype :parent-id parent-id)))


(defparameter *restaurant-options*
  (mo 0 0 "restaurant-options" 0
    (mv "ru" "Опции ресторана")
    (mv "en" "Restaurant`s options")
    (mv "it" "Elenco dei ristoranti")
    (defparameter *cuisine*
      (mo 0 0 "name" po
        (mv "ru" "Кухня")
        (mv "en" "Cuisine")
        (mv "it" "Cucina")
        (mo 0 0 "name" po
          (mv "ru" "Европейская")
          (mv "en" "European")
          (mv "it" "Europeo"))
        (mo 0 0 "name" po
          (mv "ru" "Русская")
          (mv "en" "Russian")
          (mv "it" "Russa"))
        (mo 0 0 "name" po
          (mv "ru" "Японская")
          (mv "en" "Japanese")
          (mv "it" "Giapponese"))
        (mo 0 0 "name" po
          (mv "ru" "Итальянская")
          (mv "en" "Italian")
          (mv "it" "Italiano"))
        (mo 0 0 "name" po
          (mv "ru" "Американская")
          (mv "en" "American")
          (mv "it" "Americano"))))
    (defparameter *service*
      (mo 0 0 "name" po
        (mv "ru" "Услуги")
        (mv "en" "Service")
        (mv "it" "Servizi")
        (mo 0 0 "name" po
          (mv "ru" "Завтрак")
          (mv "en" "Breakfast")
          (mv "it" "Colazione"))
        (mo 0 0 "name" po
          (mv "ru" "Ланч")
          (mv "en" "Snack")
          (mv "it" "Merenda"))
        (mo 0 0 "name" po
          (mv "ru" "Доставка")
          (mv "en" "Delivery")
          (mv "it" "Recapito"))))
    (defparameter *children*
      (mo 0 0 "name" po
        (mv "ru" "Дети")
        (mv "en" "Children")
        (mv "it" "Bambini")
        (mo 0 0 "name" po
          (mv "ru" "Детское меню")
          (mv "en" "Children menu")
          (mv "it" "Menù di bambini"))
        (mo 0 0 "name" po
          (mv "ru" "Детская комната")
          (mv "en" "Nursery room")
          (mv "it" "Stanza di bambini"))
        (mo 0 0 "name" po
          (mv "ru" "Детские праздники")
          (mv "en" "Children holiday")
          (mv "it" "Festa di bambini"))
        (mo 0 0 "name" po
          (mv "ru" "Няня")
          (mv "en" "Nanny")
          (mv "it" "Bambinaia"))))
    (defparameter *view*
      (mo 0 0 "view" po
        (mv "ru" "Вид")
        (mv "en" "View")
        (mv "it" "Vista")
        (mo 0 0 "name" po
          (mv "ru" "Панорама")
          (mv "en" "Panorama")
          (mv "it" "Panorama"))
        (mo 0 0 "name" po
          (mv "ru" "Магистраль")
          (mv "en" "Highway")
          (mv "it" "Principale"))
        (mo 0 0 "name" po
          (mv "ru" "Природа")
          (mv "en" "Nature")
          (mv "it" "Natura"))))))


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

;; (print (opt-tree *restaurant-options*))

;; <--------------------------------------

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


(defparameter *makarena*
  (make-dao
   'shop
   :code "makarena"
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
   :comment-count 89))


(let ((i *makarena*))
  (mo i 0 "name" 0
      (mv "ru" "Макарена")
      (mv "en" "Makarena"))
  (mo i 0 "description" 0
      (mv "ru" "Мы очень любим вкусно есть, вкусно пить и душевно общаться. Этим мы занимались последние несколько лет в 7 странах и более чем в 300 ресторанах. Все эти годы мы не просто наслаждались, мы вынашивали наш проект. Проект, в котором объединено все самое вкусное и интересное, что нам самим удалось попробовать в Испании, Италии, Португалии, Мексике, странах Латинской Америки и Средней Азии. Мы рады, что теперь у нас есть возможность поделиться всем этим с Вами в Санкт-Петербурге (СПб).")
      (mv "en" "we are ..."))
  (mo i 0 "phone" 0
      (mo i 0 "phone-main" po
          (mv 0 "+78129063900"))
      (mo i 0 "phone-delivery" po
          (mv 0 "+78129063900")))
  (query (:insert-into 'shop_2_subway :set 'shop-id (id i) 'subway_id (id *avtovo*)))
  (query (:insert-into 'shop_2_subway :set 'shop-id (id i) 'subway_id (id *narvskaya*)))
  (mo i 0 "street" 0
      (mv "ru" "Московский проспект")
      (mv "en" "Moscowsky prospect"))
  (mo i 0 "building" 0
      (mv "ru" "дом 206")
      (mv "en" "house 206"))
  (mo i 0 "optional" 0
      (mo i 0 "kitchen" po
          (mv "ru" "мексиканская")
          (mv "en" "mexicano")
          (mv "ru" "итальянская")
          (mv "en" "italiano"))
      (mo i 0 "service" po
          (mv "ru" "завтрак")
          (mv "en" "breakfast")
          (mv "ru" "ланч")
          (mv "en" "lunch"))
      (mo i 0 "additionally" po
          (mv "ru" "кальян")
          (mv "en" "hookah"))
      (mo i 0 "children" po
          (mv "ru" "детское меню")
          (mv "ru" "няня")
          (mv "ru" "детская комната")
          (mv "en" "children menu")
          (mv "en" "babysitter")
          (mv "en" "children room"))
      (mo i 0 "music" po
          (mv "ru" "живая")
          (mv "en" "live"))
      (mo i 0 "view" po
          (mv "ru" "панорамный")
          (mv "en" "panoramic"))))


;; (print (get-all-entityes-opt-val (select-dao 'shop)
;;                                  :entity-func #'(lambda (shop) (list :shop (id shop) (site shop)))
;;                                  :optname-func #'(lambda (option) (list :opt (id option) (parent-id option) (name option)))
;;                                  :optvalue-func #'(lambda (optval) (list :val (lang-id optval) (val optval)))))


;; (((:SHOP 1 "http://macarenabar.ru")
;;   (((:OPT 10 0 "name") ((:VAL 1 "Makarena") (:VAL 2 "Makarena")))
;;    ((:OPT 11 0 "description")
;;     ((:VAL 1 "Мы очень любим вкусно есть, вкусно пить и душевно общаться. Этим мы занимались последние несколько лет в 7 странах и более чем в 300 ресторанах. Все эти годы мы не просто наслаждались, мы вынашивали наш проект. Проект, в котором объединено все самое вкусное и интересное, что нам самим удалось попробовать в Испании, Италии, Португалии, Мексике, странах Латинской Америки и Средней Азии. Мы рады, что теперь у нас есть возможность поделиться всем этим с Вами в Санкт-Петербурге (СПб).")
;;      (:VAL 2 "we are ...")))
;;    ((:OPT 12 0 "phone") NIL)
;;    ((:OPT 13 12 "phone-main")
;;     ((:VAL 0 "+78129063900")))
;;    ((:OPT 14 12 "phone-delivery")
;;     ((:VAL 0 "+78129063900")))
;;    ((:OPT 17 0 "street")
;;     ((:VAL 1 "Московский проспект")
;;      (:VAL 2 "Moscowsky prospect")))
;;    ((:OPT 18 0 "building")
;;     ((:VAL 1 "дом 206")
;;      (:VAL 2 "house 206")))
;;    ((:OPT 19 0 "optional") NIL)
;;    ((:OPT 20 19 "kitchen")
;;     ((:VAL 1 "мексиканская")
;;      (:VAL 2 "mexicano")
;;      (:VAL 1 "итальянская")
;;      (:VAL 2 "italiano")))
;;    ((:OPT 21 19 "service")
;;     ((:VAL 1 "завтрак")
;;      (:VAL 2 "breakfast")
;;      (:VAL 1 "ланч")
;;      (:VAL 2 "lunch")))
;;    ((:OPT 22 19 "additionally")
;;     ((:VAL 1 "кальян")
;;      (:VAL 2 "hookah")))
;;    ((:OPT 23 19 "children")
;;     ((:VAL 1 "детское меню")
;;      (:VAL 1 "няня")
;;      (:VAL 1 "детская комната")
;;      (:VAL 2 "children menu")
;;      (:VAL 2 "babysitter")
;;      (:VAL 2 "children room")))
;;    ((:OPT 24 19 "music")
;;     ((:VAL 1 "живая")
;;      (:VAL 2 "live")))
;;    ((:OPT 25 19 "view")
;;     ((:VAL 1 "панорамный")
;;      (:VAL 2 "panoramic"))))))

;; (query-dao 'subway "SELECT * FROM subway WHERE id IN (SELECT subway_id FROM shop_2_subway  WHERE shop_id = 1)")


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

(defparameter *cold-dishes*
  (make-dao
   'category
   :code "cold-dishes"))

(let ((i *cold-dishes*))
  (mo i 0 "name" 0
    (mv "ru" "Холодные закуски")
    (mv "en" "Cold dishes")))

(defparameter *hot-dishes*
  (make-dao
   'category
   :code "hot-dishes"))

(let ((i *hot-dishes*))
  (mo i 0 "name" 0
    (mv "ru" "Горячие блюда")
    (mv "en" "Hot dishes")))


(query (:insert-into 'shop_2_category :set 'shop-id (id *makarena*) 'category_id (id *cold-dishes*)))
(query (:insert-into 'shop_2_category :set 'shop-id (id *makarena*) 'category_id (id *hot-dishes*)))


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

