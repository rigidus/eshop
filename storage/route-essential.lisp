(defgeneric process-route (route bindings))
(defgeneric route-submodule (route))

(defclass route (routes:route)
  ((symbol :initarg :symbol :reader route-symbol)
   (submodule :initarg :submodule :initform nil :reader route-submodule)
   (required-method :initarg :required-method :initform nil :reader route-required-method)
   (arbitrary-requirement :initarg :arbitrary-requirement :initform nil :reader route-arbitrary-requirement)
   (render-method :initarg :render-method :initform #'identity)
   (headers :initarg :headers :initform nil :reader route-headers)))

(defun route-render-method (route)
  (or (slot-value route 'render-method)
      (string-symbol-value +render-method-symbol+
                           (slot-value (slot-value route
                                                   'submodule)
                                       'module))
      #'identity))

(defmacro define-route (name (template
                              &key
                              (method :get)
                              content-type
                              render-method
                              requirement
                              validators
                              parse-vars
                              headers
                              decorators)
                        &body body)
  (let* ((template-value (if (symbolp template)
                             (symbol-value template)
                             template))
         (variables (iter (for var in (routes:template-variables (routes:parse-template template-value)))
                          (collect (list (intern (symbol-name var))
                                         (list 'cdr (list 'assoc var '*bindings*)))))))
    `(progn
       (defun ,name (,@(if variables (cons '&key variables)))
         ,@body)
       (setf (symbol-plist ',name)
             (list :template ,template
                   :method ,method
                   :content-type ,content-type
                   :validators ,validators
                   :parse-vars ,parse-vars
                   :requirement ,requirement
                   :render-method ,render-method
                   :headers ,headers
                   :decorators ,decorators))
       (intern (symbol-name ',name)
               (symbol-value (find-symbol +routes-symbol+)))
       (export ',name)
       (eval-when (:execute)
         (reconnect-all-routes)))))


(defun apply-decorators (route decorators)
  (if decorators
      (apply-decorators (funcall (car decorators)
                                 route)
                        (cdr decorators))
      route))

(defun genurl (route-symbol &rest args)
  (puri:render-uri (genurl/impl (concatenate 'list
                                             (submodule-full-baseurl *submodule*)
                                             (route-symbol-template route-symbol))
                                args)
                   nil))