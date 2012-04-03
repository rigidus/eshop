;;;; defmodule.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(restas:define-module #:eshop.storage
    (:use #:closer-mop #:cl #:iter #:alexandria #:anaphora #:json #:postmodern)
  (:shadowing-import-from :closer-mop
                          :defclass
                          :defmethod
                          :standard-class
                          :ensure-generic-function
                          :defgeneric
                          :standard-generic-function
                          :class-name))

(in-package #:eshop.storage)

(defparameter *basedir*
  (asdf:component-pathname (asdf:find-system '#:eshop.storage)))

(defun path (relative)
  (merge-pathnames relative *basedir*))

