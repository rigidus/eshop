;;;; lib.lisp
;;;;
;;;; This file is part of the cl-eshop project, released under GNU Affero General Public License, Version 3.0
;;;; See file COPYING for details.
;;;;
;;;; Author: Glukhov Michail aka Rigidus <i.am.rigidus@gmail.com>

(in-package #:eshop.storage)


(defmacro def~class ((class-name super-class-names) &body slots)
  `(prog1
       (defclass ,class-name ,super-class-names
         ,(loop :for (slot-name initform) :in slots :collect
             `(,slot-name :initarg  ,(intern (symbol-name slot-name) :keyword)
                          :initform ,initform
                          :accessor ,(intern (format nil "A-~A" (symbol-name slot-name))))))
     ;; (defmethod print-object ((obj ,class-name) stream)
     ;;   (format stream
     ;;           (format nil "#[ ~A | ~A]"
     ;;                   ',class-name
     ;;                   (loop :for slot :in (closer-mop:class-slots (find-class ',class-name)) :collect
     ;;                      (format nil ":~A ~A"
     ;;                              (closer-mop:slot-definition-name slot)
     ;;                              (bprint (funcall (intern (format nil "A-~A" (symbol-name (closer-mop:slot-definition-name slot)))) obj)))))))
     ))


(defmacro mi (class-name &body body)
  (with-gensyms (new id)
    (cond ((or (equal class-name ''supplier)
               (equal class-name ''builder)
               (equal class-name ''individual))
           `(let* ((,new  (make-instance ,class-name ,@body))
                   (,id   (store ,new)))
              (setf (gethash ,id *USER*) ,new)
              (values ,new ,id)))
          (t `(make-instance ,class-name ,@body)))))

