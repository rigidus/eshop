

;;;; пример
;;server {
;;   listen   80;
;;    location / {
;;      proxy_pass      http://127.0.0.1:4242;
;;      proxy_redirect  off;
;;   }
;;}
;;================================================================
(ql:quickload '(#:postmodern #:cl-json #:restas))

(restas:define-module #:restas.testdb
  (:use #:cl #:postmodern)
;;;  (:export ..)
           )

(in-package #:restas.testdb)

(defvar *spec-db* '("ravtadb" "ravta" "ravta1111" "localhost"))
(defvar *empty-result* "нихрена не нашлось :(")
(defvar *price-max* 999999)
(defvar *price-max* most-positive-fixnum)

json::(defun write-json-chars (s stream)
  "Write JSON representations (chars or escape sequences) of
characters in string S to STREAM."
  (loop for ch across s
     for code = (char-code ch)
     with special
     if (setq special (car (rassoc ch json::+json-lisp-escaped-chars+)))
     do (write-char #\\ stream) (write-char special stream)
     else if (< #x1f code #x7f)
     do (write-char ch stream)
     else
     do (let ((special '#.(rassoc-if #'consp json::+json-lisp-escaped-chars+)))
          (destructuring-bind (esc . (width . radix)) special
            ;; (format stream "\\~C~V,V,'0R" esc radix width code)
            (write-char ch stream)
            ))))


(restas:define-route main ("")
  (format nil "{ \"response\": ~A }"
          *empty-result*))

(restas:define-route product ("product")
  (let ((pr-min (hunchentoot:get-parameter "price-min"))
        (pr-max (hunchentoot:get-parameter "price-max"))
        resp-list)
    (unless pr-min (setf pr-min 0))
    (unless pr-max (setf pr-max *price-max*))
    (setf resp-list (with-connection *spec-db*
                      (query (:select 'name 'price
                                      :from 'products
                                      :where (:and (:>= 'price '$1)
                                                   (:<= 'price '$2)))
                             :alists
                             (format nil "руб~A" pr-min)
                             (format nil "руб~A" pr-max))))
    (format nil "{ \"response\": ~A }"
            (if resp-list
                (json:encode-json-to-string resp-list)
                *empty-result*))))

(restas:define-route group ("group")
  (let (resp-list)
    (setf resp-list (with-connection *spec-db*
                      (query (:select 'name
                                      :from 'p_groups)
                             :alists)))
    (format nil "{ \"response\": ~A }"
            (if resp-list
                (json:encode-json-to-string resp-list)
                *empty-result*))))


(restas:start '#:restas.testdb :port 4242 :address "localhost")
(restas:debug-mode-on)

