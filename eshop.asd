;;;; defsystem.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(asdf:defsystem #:eshop
  :version      "11.03.2011"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "GPLv3"
  :description  "eshop"
  :depends-on   (#:closer-mop
                 #:cl-ppcre
                 #:restas-directory-publisher
                 #:closure-template
                 #:cl-json)
  :serial       t
  :components   ((:file "defmodule")
                 (:static-file "daemon.conf")
                 (:static-file "daemon.lisp")
                 (:static-file "daemon.sh")))
