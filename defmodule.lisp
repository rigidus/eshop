(require 'restas)

(restas:define-module #:eshop
    (:use #:cl #:iter #:alexandria))

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
