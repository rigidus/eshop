;;;; render
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop.web)


(defclass eshop.web-render () ())

(setf *default-render-method* (make-instance 'eshop.web-render))

(defmethod restas:render-object ((designer eshop.web-render) (obj t))
  "stub")
