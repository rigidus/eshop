;;;; render.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop.storage)

;; default-render

(defclass storage-render () ())

(setf *default-render-method* (make-instance 'storage-render))

(defmethod restas:render-object ((designer storage-render) (data list))
  "default render stub")

(defmethod restas:render-object ((designer storage-render) (obj resto))
  (format nil "{\"response\": ~A}" (encode-json-to-string obj)))


