;;;; init.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop.storage)


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
(progn
  (defparameter *rus* (make-dao 'country :code "rus"))
  (let ((opt (make-option *rus* 0 "name")))
    (make-value opt "ru" "Россия")
    (make-value opt "en" "Russia")
    (make-value opt "it" "Rùssia")))

(progn
  (defparameter *usa* (make-dao 'country :code "usa"))
  (let ((opt (make-option *usa* 0 "name")))
    (mv "ru" "США")
    (mv "en" "USA")
    (mv "it" "Stati Uniti d'America")))

(progn
  (defparameter *ita* (make-dao 'country :code "ita"))
  (mo *ita* 0 "name" 0
    (mv "ru" "Италия")
    (mv "en" "Italy")
    (mv "it" "Itàlia")))

;; (val (car (load-values (car (load-options (car (select-dao 'country)) :name "name" :lang 0)) :lang "ru")))


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



;; (print (opt-tree *restaurant-options*))

;; ("Опции ресторана"
;;  (("Кухня"
;;    (("Европейская") ("Русская") ("Японская") ("Итальянская") ("Американская")))
;;   ("Услуги"
;;    (("Завтрак") ("Ланч") ("Доставка")))
;;   ("Дети"
;;    (("Детское меню") ("Детская комната") ("Детские праздники") ("Няня")))
;;   ("Вид"
;;    (("Панорама") ("Магистраль") ("Природа")))))

;; <--------------------------------------



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


(defparameter *sandwich*
  (make-dao
   'product
   :code "sandwich"
   :price 20))

(let ((i *sandwich*))
  (mo i 0 "name" 0
    (mv "ru" "Сэндвич")
    (mv "en" "Sandwich")))


(query (:insert-into 'category_2_product :set 'category-id (id *cold-dishes*) 'product-id (id *sandwich*)))
(query (:insert-into 'shop_2_product :set 'shop-id (id *makarena*) 'product-id (id *sandwich*)))


(defparameter *gamburger*
  (make-dao
   'product
   :code "gamburger"
   :price 30))

(let ((i *gamburger*))
  (mo i 0 "name" 0
    (mv "ru" "Гамбургер")
    (mv "en" "Gamburger")))

(query (:insert-into 'category_2_product :set 'category-id (id *hot-dishes*) 'product-id (id *gamburger*)))
(query (:insert-into 'shop_2_product :set 'shop-id (id *makarena*) 'product-id (id *gamburger*)))

(defparameter *sosiska*
  (make-dao
   'product
   :code "sosiska"
   :price 30))

(let ((i *sosiska*))
  (mo i 0 "name" 0
    (mv "ru" "Сосиска")
    (mv "en" "Sosiska")))

(query (:insert-into 'category_2_product :set 'category-id (id *hot-dishes*) 'product-id (id *sosiska*)))
(query (:insert-into 'shop_2_product :set 'shop-id (id *makarena*) 'product-id (id *sosiska*)))



