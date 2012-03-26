(require 'closer-mop)
(require 'restas)
(require 'closure-template)
(require 'restas-directory-publisher)
(require 'cl-base64)
(require 'cl-json)
;; (require 'drakma)


(restas:define-module #:eshop
    (:use #:closer-mop
          #:cl
          #:iter
          #:alexandria
          #:json)
  (:shadowing-import-from :closer-mop
                          :defclass
                          :defmethod
                          :standard-class
                          :ensure-generic-function
                          :defgeneric
                          :standard-generic-function
                          :class-name))

(in-package #:eshop)

(setf asdf:*central-registry*
      (remove-duplicates (append asdf:*central-registry*
                                 (mapcar #'(lambda (path)
                                             (merge-pathnames
                                              (make-pathname :directory `(:relative ,path))
                                              (user-homedir-pathname)))
                                         '("repo/eshop"
                                           "repo/eshop/perm"
                                           "repo/eshop/gate"
                                           "repo/eshop/web"
                                           "repo/eshop/storage")))
                         :test #'equal))

(defparameter *basedir*
  (asdf:component-pathname (asdf:find-system '#:eshop)))

(defun path (relative)
  (merge-pathnames relative *basedir*))

(asdf:operate 'asdf:load-op '#:eshop.perm)
(asdf:operate 'asdf:load-op '#:eshop.gate)
(asdf:operate 'asdf:load-op '#:eshop.web)
(asdf:operate 'asdf:load-op '#:eshop.storage)
