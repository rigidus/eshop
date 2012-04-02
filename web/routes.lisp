(in-package #:eshop.web)
(defparameter
 content-defaults-en
 '(
   ;; content structure
   ;; FIXME add real data
   (:data :html "Demo text")
   (:meta
    :site (:name "eshop" :slogan "we can do it all"
           :title "e-commerce oil eshop catalog download free seo"
           :copyright "2012 () Copyleft"
           :meta ((:name "keywords"
                   :content "e-commerce, oil, eshop, catalog, download, free")
                  (:http-equiv "X-UA-Compatible" :content "IE=edge")))
    :page (:id "index" :slug "root" :title "Welcome" :name "Welcome to eshop"
           :lang "en-UK"
           :meta ((:name "author"
                   :content "Me")
                  (:name "description" :content "welcome mesage")))
    :charset "utf-8"
    :class ("lisp" "ajax"))
   (:app
    :src "app.js" :name "app"
    :data (("main" "scripts/main") ("version" "git-20120402")))
   (:nav
    :items ((:link "/" :caption "Home") (:link "about" :caption "About us"))
    :id "global" )))

(restas:define-route index ("")
   content-defaults-en)


(restas:define-route product ("product/:uid"
                              :parse-vars (list :uid #'parse-integer)))