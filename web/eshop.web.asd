;;;; eshop.web.asd
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(asdf:defsystem #:eshop.web
  :version      "11.03.2011"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "GPLv3"
  :description  "eshop"
  :depends-on   (#:cl-ppcre
                 #:restas-directory-publisher
                 #:closure-template)
  :serial       t
  :components   ((:file "defmodule")
                 (:module "tpl"
                  :components ((:static-file "templates.htm")
                               (:static-file "default.soy")))
                 (:file "render")
                 (:file "routes")
                 (:file "init")))
