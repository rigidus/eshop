;;;; render.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop.web)

(defclass eshop.web.render () ())

(setf *default-render-method* (make-instance 'eshop.web.render))

(defmethod render-route-data
    ((designer eshop.web.render) (data list) route)
  (funcall
   (find-symbol (symbol-name route) '#:eshop.web.design.default)
   data))

(defmethod restas:render-object
    ((designer eshop.web.render) (data list))
  (render-route-data designer data (restas:route-symbol restas:*route*)))

(compile-cl-templates 
 (mapcar 'asdf:component-pathname
         (asdf:module-components
          (asdf:find-component (asdf:find-system '#:eshop.web) 'tpl))))