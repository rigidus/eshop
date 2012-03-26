

(in-package #:eshop.storage)


;; main

(restas:define-route main ("/")
  "manifest stub")

(restas:define-route rest-list ("/restaurant")
  (encode-json-to-string *h-restaurant*))

(restas:define-route rest-id ("/restaurant/:id")
  (gethash (parse-integer id :junk-allowed t) *h-restaurant*))


;; (json:encode-json-to-string ...)
