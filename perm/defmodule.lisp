;;;; defmodule.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(restas:define-module #:eshop.perm
    (:use #:cl #:iter #:alexandria))

(in-package #:eshop.perm)

(defparameter *basedir*
  (asdf:component-pathname (asdf:find-system '#:eshop.perm)))

(defun path (relative)
  (merge-pathnames relative *basedir*))

