(in-package #:eshop.web)

(defun menu ()
  (list (list :link "/" :title "Главная")
        (list :link "/about" :title "About")
        (list :link "/contacts" :title "Контакты")))



;; start
(restas:start '#:eshop.web :port 8082)
(restas:debug-mode-on)
;; (restas:debug-mode-off)
(setf hunchentoot:*catch-errors-p* t)
