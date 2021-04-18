(defpackage :rin-template
  (:use :cl :rin-util)
  (:import-from :cl-ppcre
                #:split
                #:create-scanner
                #:scan
                #:quote-meta-chars)
  (:export #:add-template
           #:execute
           #:clear-functions
           #:remove-function
           #:pprint-tpl
           #:*function-package*
           #:*escape-type*))
(in-package :rin-template)

(defpackage :rin-template-intern (:use :cl))

;; Template compiled function class

(defclass tpl-function ()
  ((path :initarg :path
         :accessor tpl-function-path)
   (time :initarg :time
         :accessor tpl-function-time)
   (code :initarg :code
         :accessor tpl-function-code)
   (form :initarg :form
         :initform nil
         :accessor tpl-function-form)))

(defun make-function (path time code form)
  (make-instance 'tpl-function
                 :path path
                 :time time
                 :code code
                 :form form))

;; Package specified variables

(defvar *function-package* (find-package :rin-template-intern)
  "Package the emb function body gets interned to.")

(defvar *tpl-starter* "{%"
  "Start of a script block")

(defvar *tpl-ender* "%}")

(defvar *functions* (make-hash-table :test #'equal))

(defvar *escape-type* :raw
  "How to escape special characters in variable")

;; Escape char in string

(defun escape (string &key (escape *escape-type*))
  "Emit given string. Escape if wanted"
  (let ((true-str (or string "")))
    (case escape
      ((:html :xml)
       (escape-for-xml true-str))
      ((:url)
       (escape-for-url true-str))
      (otherwise true-str))))

;; Get variable in environment

(defgeneric getf* (ty key &optional default)
  (:documentation "Get a value from some data structure by key")
  (:method ((plist list) key &optional default)
    (getf plist key default))
  (:method ((table hash-table) key &optional default)
    (gethash key table default))
  (:method ((object standard-object) key &optional default)
    (let ((slot-name (intern (princ-to-string key)
                             (symbol-package (class-name (class-of object))))))
      (if (and (slot-exists-p object slot-name)
               (slot-boundp object slot-name))
          (slot-value object slot-name)
          default))))

(defun string-to-keyword (string)
  (nth-value 0 (intern (string-upcase string) :keyword)))

(defmacro getf-env (key)
  (let ((plist (if (char= (char key 0) #\/)
                   (find-symbol "TOPENV" rin-template:*function-package*)
                   (find-symbol "ENV" rin-template:*function-package*)))
        (keys (split "/" key :sharedp t)))
    (labels ((plist-find (plist keys)
               (if (null keys)
                   plist
                   (plist-find
                    (if (zerop (length (first keys)))
                        plist
                        `(getf* ,plist ,(string-to-keyword (first keys))))
                    (rest keys)))))
      (plist-find plist keys))))

;;; (Re)Compile functions from template

(let ((scanner-hash (make-hash-table :test #'equal)))
  (defun scanner-for-expand-tag (tag)
    "Memorize a scanner of definite tags"
    (or (gethash tag scanner-hash)
        (setf (gethash tag scanner-hash)
              (create-scanner tag))))
  (defun clear-tag-hash ()
    "Remove all scanners from cache"
    (clrhash scanner-hash)))

(defparameter *tag-regexp*
  `(("\\s+@if\\s+(\\S+)\\s*" . " (cond ((rin-template::getf-env \"\\1\") ")
    ("\\s+@else\\s*" . " ) (t ")
    ("\\s+@endif\\s*" . " )) ")
    ("\\s+@repeat\\s+(\\d+)\\s*" . " (dotimes (i \\1) ")
    ("\\s+@repeat\\s+(\\S+)\\s*" . " (dotimes (i (or (rin-template::getf-env \"\\1\") 0)) ")
    ("\\s+@endrepeat\\s*" . " ) ")
    ("\\s+@for\\s+(\\S+)\\s*" . " (dolist (env (rin-template::getf-env \"\\1\")) ")
    ("\\s+@endfor\\s*" . " ) ")
    ("=?\\s+@var\\s+(\\S+)\\s+-(\\S+)\\s+(\\S+)\\s*" . "= (rin-template::escape (rin-template::getf-env \"\\1\") :\\2 :\\3) ")
    ("=?\\s+@var\\s+(\\S+)\\s*" . "= (rin-template::escape (rin-template::getf-env \"\\1\")) ")
    ("\\s+@with\\s+(\\S+)\\s*" . " (let ((env (rin-template::getf-env \"\\1\"))) ")
    ("\\s+@endwith\\s*" . " ) ")
    ("\\s+@include\\s+(\\S+)\\s*" . "= (let ((rin-template:*escape-type* rin-template:*escape-type*))
                                            (rin-template:execute (merge-pathnames \"\\1\" template-path-default) :env env)) ")
    ("\\s+@call\\s+(\\S+)\\s*" . "= (let ((rin-template:*escape-type* rin-template:*escape-type*))
                                            (rin-template:execute \"\\1\" :env env)) "))
  "A list of regex and replacement")

(defun expand-tags (string)
  "Expand special tags begin with @ to lisp"
  (labels ((expand (string &optional (expands *tag-regexp*))
             (let ((regex (scanner-for-expand-tag
                           (concatenate 'string "(?is)" "^" (first (first expands)) "$")))
                   (replacement (rest (first expands))))
               (if (null (rest expands))
                   (ppcre:regex-replace-all regex string replacement :simple-calls t)
                   (expand (ppcre:regex-replace-all regex string replacement :simple-calls t)
                           (rest expands))))))
    (ppcre:regex-replace-all (format nil "(?is)(~A\\-?)(.+?)(\\-?~A)"
                                     (quote-meta-chars *tpl-starter*)
                                     (quote-meta-chars *tpl-ender*))
                             string
                             (lambda (match start-tag string end-tag)
                               (declare (ignore match))
                               (if (scan "(?is)^#.+#$" string)  ; is comment
                                   ""
                                   (concatenate 'string start-tag (expand string) end-tag)))
                             :simple-calls t)))

(defun tag-format (tag-type)
  "Return the format string depends on whether it is
:script or :expr"
  (ecase tag-type
    ((:script) "~A")
    ((:expr) "(format t \"~~A\" ~A)")))

(defun next-tag (string start)
  "Postions and type of next tag"
  (let ((start-tag (search *tpl-starter* string :start2 start)))
    (if (not start-tag)
        nil
        (let ((start-code (+ start-tag (length *tpl-starter*))))
          (case (and (> (length string) start-code)
                     (char string start-code))
            (#\= (values start-tag (1+ start-code) :expr nil))
            (#\- (values start-tag (1+ start-code) :script t))
            (otherwise (values start-tag start-code :script nil)))))))

(defun construct-function-body (code &optional (start 0))
  "Take a expanded template and return original lisp code"
  (multiple-value-bind (start-tag start-code tag-type trim-start)
      (next-tag code start)
    (if (not start-tag)
        (format nil "(write-string ~S)" (subseq code start))
        (let* ((end-code (search *tpl-ender* code :start2 start-code))
               (trim-end (char= (char code (1- end-code)) #\-)))
          (if (not end-code)
              (error "tag '~A' is not enclosed." *tpl-starter*)
              (format nil "(write-string ~S) ~A ~A"
                      (if trim-start
                          (trim-till-newline (subseq code start start-tag))
                          (subseq code start start-tag))
                      (format nil (tag-format tag-type)
                              (subseq code start-code (if trim-end
                                                          (1- end-code)
                                                          end-code)))
                      (construct-function-body
                       code
                       (if trim-end
                           (let ((next-pos (scan "(?:\\S|\\n)" code :start (+ end-code (length *tpl-ender*)))))
                             (cond
                               ((null next-pos) (length code))
                               ((char= (elt code next-pos) #\Newline)
                                (1+ next-pos))
                               (t next-pos)))
                           (+ end-code (length *tpl-ender*))))))))))

(defun construct-function (code)
  "Compile a lisp function from template"
  (let ((form
         `,(let ((*package* *function-package*))
               (read-from-string
                (format nil "(lambda (&key env name)
                               (declare (ignorable env))
                               (let ((template-path-default (if (typep name 'pathname) name *default-pathname-defaults*))
                                     (topenv env))
                                 (declare (ignorable template-path-default topenv))
                                 (with-output-to-string (*standard-output*)
                                 (progn ~A))))"
                        (construct-function-body (expand-tags code)))))))
    (values (compile nil form) form)))

(defun get-function (name)
  "Get generated function in hash table.
Recompile if file has been modified."
  (let* ((function (gethash name *functions*))
         (path (when function (tpl-function-path function))))
    (cond ((and (not (typep name 'pathname)) (null function))
           (error "no such function ~S." name))
          ((null function)
           (return-from get-function))
          ((and path
                (> (file-write-date path) (tpl-function-time function)))
            (multiple-value-bind (code form)
                (construct-function (contents path))
             (setf (tpl-function-time function) (file-write-date path)
                   (tpl-function-code function) code
                   (tpl-function-form function) form))))
    (tpl-function-code function)))

;; Functions exported

(defgeneric add-template (name template)
  (:documentation "Add a new template into hash table")
  (:method (name (template string))
    (multiple-value-bind (code form)
        (construct-function template)
      (setf (gethash name *functions*)
            (make-function nil
                           (get-universal-time)
                           code
                           form))))
  (:method (name (template pathname))
    (multiple-value-bind (code form)
        (construct-function (contents template))
      (setf (gethash name *functions*)
            (make-function template
                           (file-write-date template)
                           code
                           form)))))

(defgeneric execute (name &key env)
  (:documentation "Execute a generated function with environment")
  (:method ((name t) &key env)
    (funcall (get-function name) :env env :name name))
  (:method ((name pathname) &key env)
    (let ((fun (or (get-function name)
                   (tpl-function-code (add-template name name)))))
      (funcall fun :env env :name name))))

(defun clear-functions ()
  (clrhash *functions*))

(defun remove-function (name)
  (remhash name *functions*))

(defun pprint-tpl (name)
  (pprint (tpl-function-form (gethash name *functions*))))

;;; template.lisp ends here
