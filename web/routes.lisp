(in-package #:eshop.web)

(restas:define-route index ("")
  '(
   ;; FIXME add real data
    (:data :text "Demo text" :id "demo")
    (:meta :title "WELCOME TO ESHOP" :id "index" :class ("lisp" "ajax"))))

(restas:define-route product ("product/:uid"
                              :parse-vars (list :uid #'parse-integer)))