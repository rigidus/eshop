(ql:quickload '(#:postmodern #:cl-json #:sb-fastcgi ))

(defpackage #:test
  (:use #:cl
        #:postmodern
        #:json
        #:sb-fastcgi))

(in-package #:test)

(sb-fastcgi:load-libfcgi)


json::(defun write-json-chars (s stream)
  "Write JSON representations (chars or escape sequences) of
characters in string S to STREAM."
  (loop for ch across s
     for code = (char-code ch)
     with special
     if (setq special (car (rassoc ch json::+json-lisp-escaped-chars+)))
     do (write-char #\\ stream) (write-char special stream)
     else if (< #x1f code #x7f)
     do (write-char ch stream)
     else
     do (let ((special '#.(rassoc-if #'consp json::+json-lisp-escaped-chars+)))
          (destructuring-bind (esc . (width . radix)) special
            ;; (format stream "\\~C~V,V,'0R" esc radix width code)
            (write-char ch stream)
            ))))

(defvar *spec-db* '("ravtadb" "ravta" "ravta1111" "localhost"))
(defvar *empty-result* "нихрена не нашлось :(")

(defun chmod-socket (sock-path &optional (permission "777"))
  (unless (sb-impl::process-exit-code
           (sb-ext:run-program "chmod" (list permission sock-path)
                               :search t))
    (format t "ОШИБКА СМЕНЫ ПРАВ СОКЕТА")))

sb-fastcgi::(defun server-on-fd-threaded (func fd &key (flags 0) (threads 4))
  (fcgx-init)
  (do ((count 0 (1+ count)))
      ((>= count threads) 'THREADS-START-DONE)
    (sb-thread:make-thread (lambda ()
                             (server-on-fd func fd :flags flags))
                           :name (format nil "тред-~A" count))))

(defun destroy-sockets-thread (&optional (th-name "тред-0"))
  (mapc #'(lambda (x)
          (when (equal th-name (sb-thread:thread-name x))
           (sb-thread:destroy-thread x)))
      (sb-thread:list-all-threads)))

(defun wsgi-app (env start-response)
;        (declare (ignore env))
        (funcall start-response "200 OK" '(("X-author" . "Who?")
                                           ("Content-Language" . "ru")
                                           ("Content-Type" . "text/html; charset=utf-8")))
        (let ((req-str (subseq (cdr (assoc "script_filename" env :test #'equalp)) 22))
              (req-par (cdr (assoc "query_string" env :test #'equalp)))
              resp-r
              resp-n)
          (declare (ignore req-par))
          (cond
            ((equalp req-str "group")
             (multiple-value-bind (result-rows result-nums)
                 (with-connection *spec-db*
                   (query (:select 'name :from 'p_groups) :alists))
               (setf resp-r result-rows resp-n result-nums)))
            ((equalp req-str "product")
             (multiple-value-bind (result-rows result-nums)
                 (with-connection *spec-db*
                   (query (:select 'name 'price :from 'products) :alists))
               (setf resp-r result-rows resp-n result-nums)))
            (t
             (setf resp-r nil resp-n 0)))
          (list (format nil "{ \"response\": ~A }"
                  (if (plusp resp-n)
                      (json:encode-json-to-string resp-r)
                      *empty-result*)))))


(defun run-app-1 ()
  (sb-fastcgi:socket-server-threaded
   (sb-fastcgi:make-serve-function #'wsgi-app)
   ;; :inet-addr "127.0.0.1")) ; или сокет или инет
   :sock-path "/tmp/sb-fastcgi.sock"
   :threads 1))

(run-app-1)

(chmod-socket "/tmp/sb-fastcgi.sock") ; после (run-app-1)

