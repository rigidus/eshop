;;;; render.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop.web)

(defclass eshop.web.render () ())

(compile-cl-templates 
 (mapcar 'asdf:component-pathname
         (asdf:module-components
          (asdf:find-component (asdf:find-system '#:eshop.web) 'tpl))))

(setf *default-render-method* (make-instance 'eshop.web.render))
(setf *default-design* '#:eshop.web.design.default)

(defmethod restas:render-object
    ((designer eshop.web.render) (data list))
  (let* ((route (symbol-name (restas:route-symbol restas:*route*)))
         (page-designer (find-symbol (symbol-name 'page) *default-design*))
         (content-designer
           (or
            (find-symbol route *default-design*) 
            (find-symbol (symbol-name 'default-layout ) *default-design*)))
         (content-html (funcall content-designer data))
         (data (append data (list (list :innerhtml content-html)) nil)))
    (funcall page-designer data)))
