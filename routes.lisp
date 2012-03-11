(in-package #:eshop)

(restas:define-route request-static-route-img ("/img/*")
  (let ((full-uri (format nil "~a" (restas:request-full-uri))))
    (pathname (concatenate 'string *basedir* "/img/" (subseq full-uri (search "/img/" full-uri))))))

;; (restas:define-route request-static-route-pic ("/pic/*")
;;     (let* ((full-uri (format nil "~a" (restas:request-full-uri)))
;;                     (path-to-img (ppcre:regex-replace ".*/pic/(\\w{1,})/(\\d{1,3})(\\d{0,})/(.*)$" full-uri "\\1/\\2/\\2\\3/\\4")))
;;           (pathname (format nil "~a/~a" *path-to-product-pics* path-to-img))))

;; (restas:define-route request-static-route-css ("/css/*")
;;     (let ((full-uri (format nil "~a" (restas:request-full-uri))))
;;           (pathname (concatenate 'string *path-to-dropbox* "/htimgs/" (subseq full-uri (search "/css/" full-uri))))))

;; (restas:define-route request-static-route-js ("/js/*")
;;     (let ((full-uri (format nil "~a" (restas:request-full-uri))))
;;           (pathname (concatenate 'string *path-to-dropbox* "/htimgs/" (subseq full-uri (search "/js/" full-uri))))))

;; (restas:define-route request-route-static-favicon ("/favicon.ico")
;;       (pathname (concatenate 'string *path-to-dropbox* "/htimgs/img/favicon.ico")))

;; (restas:define-route request-route-static-robots ("/robots.txt")
;;       (pathname (concatenate 'string *path-to-conf* "/robots.txt")))

;; (restas:define-route request-route-static-yml ("/yml.xml")
;;       (pathname (concatenate 'string *path-to-conf* "/yml.xml")))

;; (restas:define-route request-route-static-sitemap ("/sitemap.xml")
;;       (pathname (concatenate 'string *path-to-conf* "/sitemap.xml")))

;; (restas:define-route request-route-static-sitemap-index ("/sitemap-index.xml")
;;       (pathname (concatenate 'string *path-to-conf* "/sitemap-index.xml")))

;; (restas:define-route request-route-static-sitemap1 ("/sitemap1.xml")
;;       (pathname (concatenate 'string *path-to-conf* "/sitemap1.xml")))

;; (restas:define-route request-route-static-sitemap2 ("/sitemap2.xml")
;;       (pathname (concatenate 'string *path-to-conf* "/sitemap2.xml")))
