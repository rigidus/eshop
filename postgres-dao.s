;; ssh -2Nf -L 4005:localhost:4005 192.168.1.11

;; опции класса: :table-name (по умолчанию используется имя класса),
;;               :keys для предоставления множества столбцов для первичного ключя таблицы.
;; Если первичного ключа нет, такие операции, как update-dao и get-dao не будут работать.
;; Форма (or db-null integer) для столбца, который может иметь значения NULL.

(defvar *spec-db* '("ravtadb" "ravta" "ravta1111" "localhost"))
(defvar *empty-result* "нихрена не нашлось :(")
(defvar *price-max* 536870911 "32-разрядное most-positive-fixnum")
(defvar *price-max* most-positive-fixnum)



(defvar *prod-id* 0
  "Переменная автоинкремента для product")

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

(defvar *opname-id* 0
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

(defvar *option-id* 0
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

(dao-table-definition 'product)
(dao-table-definition 'opname)



