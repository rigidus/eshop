(require 'restas)

(restas:define-module #:eshop
    (:use #:cl #:iter #:alexandria))

(in-package #:eshop)

(let ((path          '(:RELATIVE "repo/eshop"))
      (web-path      '(:RELATIVE "repo/eshop/web"))
      (storage-path  '(:RELATIVE "repo/eshop/storage")))
  (setf asdf:*central-registry*
        (remove-duplicates (append asdf:*central-registry*
                                   (list (merge-pathnames
                                          (make-pathname :directory path)
                                          (user-homedir-pathname))
                                         (merge-pathnames
                                          (make-pathname :directory web-path)
                                          (user-homedir-pathname))
                                         (merge-pathnames
                                          (make-pathname :directory storage-path)
                                          (user-homedir-pathname))))
                           :test #'equal)))

(defparameter *basedir*
  (asdf:component-pathname (asdf:find-system '#:eshop)))

(defun path (relative)
  (merge-pathnames relative *basedir*))

