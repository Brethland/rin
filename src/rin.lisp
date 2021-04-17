(in-package :rin)

(defconstant +cfg-msg+ ";;; This is config file for your site.~2%(site-name example.com)")

(defun get-cwd ()
  "Get the current directory pathname in an implementation-portable way"
  (let ((dir (truename ".")))
    (if (stringp dir)
        (parse-namestring dir)
        dir)))

(defun create-project (dir-name)
  "Create a new empty project in given path"
  (let* ((user-path (get-cwd))
         (path (merge-pathnames (concatenate 'string dir-name "/") user-path)))
    (ensure-directories-exist path)
    (ensure-directories-exist (merge-pathnames #p"post/" path))
    (ensure-directories-exist (merge-pathnames #p"static/" path))
    (ensure-directories-exist (merge-pathnames #p"style/" path))
    (with-open-file (stream (merge-pathnames #p"config" path)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream +cfg-msg+))
    (format t "Your site has been created!~%")))

(defun generate-opts ()
  "Generate command line arguments parser"
  (opts:define-opts
      (:name :help
             :description "help"
             :short #\h
             :long "help")
      (:name :init
             :description "init a new site with given name"
             :short #\i
             :long "init"
             :arg-parser #'identity)))

(defun unknown-option (condition)
  "Error handler for opts:unknown-option"
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun missing-arg (condition)
  "Error handler for opts:missing-arg"
  (format t "fatal: option ~s needs an argument!~%"
    (opts:option condition))
  (uiop:quit -1))

(defun arg-parser-failed (condition)
  "Error handler for opts:arg-parser-failed"
  (format t "fatal: cannot parse ~s as argument of ~s~%"
    (opts:raw-arg condition)
    (opts:option condition))
  (uiop:quit -1))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun main (&rest args)
  "Entry of rin"
  (declare (ignore args))
  (generate-opts)
  (multiple-value-bind (options free-args)
    (handler-bind ((opts:unknown-option #'unknown-option)
                   (opts:missing-arg #'missing-arg)
                   (opts:arg-parser-failed #'arg-parser-failed))
          (opts:get-opts))
    (when-option (options :help)
      (opts:describe
        :prefix "rin, a fast static site generator")
      (uiop:quit 0))
    (when-option (options :init)
      (create-project it)
      (uiop:quit 0))
    (when free-args
      (format t "unexpected args: ~a~2%"
        free-args))
    (format t "use -h to see help~%")
    (uiop:quit -1)))

;;; rin.lisp ends here
