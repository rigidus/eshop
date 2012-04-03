;;;; defmodule
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(restas:define-module #:eshop.gate
    (:use #:cl #:iter #:alexandria))

(in-package #:eshop.gate)

(defparameter *basedir*
  (asdf:component-pathname (asdf:find-system '#:eshop.gate)))

(defun path (relative)
  (merge-pathnames relative *basedir*))

