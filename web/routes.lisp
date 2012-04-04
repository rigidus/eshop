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
                  (:http-equiv "X-UA-Compatible" :content "IE=edge"))
           :nav ( :global ( :id "global"
                            :items ((:link "/" :caption "HOME")
                                    (:link "products" :caption "Catalog"
                                     :items
                                           ((:link "products/oil" :caption "OIL")
                                            (:caption "coil" :link "products/coil")))
                                    (:link "about" :caption "about Us")))))
    :page (:id "index" :slug "/" :title "Welcome" :name "Welcome to eshop"
           :lang "en-UK"
           :meta ((:name "author"
                   :content "Me")
                  (:name "description" :content "welcome mesage")))
    :charset "utf-8"
    :class ("lisp" "ajax"))
   (:app
    :src "require-jquery.js" :name "app"
    :data (("main" "main.js") ("version" "git-20120402")))))

(restas:define-route index
    ("")
   content-defaults-en)

(restas:define-route design-style
    ("design/:(design).css"
     :render-method (make-instance 'eshop.web.css)
     :content-type "text/css")
  design)

(restas:define-route app-js
    (":(script).js"
     :content-type "application/javascript")
  (path (concatenate 'string "app/" script ".js")))

(restas:define-route submodule-js
    (":(module)/:(script).js"
     :content-type "application/javascript")
  (path (concatenate 'string
                     (if (string= module "app")
                         "app"
                         (concatenate 'string "app/" module))
                     "/" script ".js")))

(restas:define-route product ("product/:uid"
                              :parse-vars (list :uid #'parse-integer)))
