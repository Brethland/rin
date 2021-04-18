(in-package :rin)

(defvar *site-config* nil)

(defclass config ()
  ((domain :initarg :domain :reader domain)
   (title :initarg :title :reader title)
   (author :initarg :author :reader author)
   (doc-type :initarg :doc-type :reader doc-type)
   (excerpt-sep :initarg :excerpt-sep :reader excerpt-sep))
  (:default-initargs :excerpt-sep "<!--more-->"))

(defun load-config ()
  "Load config file into *site-config*"
  (with-open-file (in (merge-pathnames #p"config" (getcwd))
                      :external-format :utf-8
                      :if-does-not-exist :error)
    (let ((config (read in)))
      (setf *site-config* (apply 'make-instance 'config config)))))

(defvar *site* (make-hash-table :test #'equal)
  "Use urls to index object loaded from repo")

(defgeneric url-of (obj)
  (:documentation "Get url of OBJ"))

(defun find-all (class)
  "Find all objects of CLASS in *site*"
  (loop for val being the hash-values in *site*
        when (typep val class) collect val))

(defun delete-all (class)
  "Delete all objects of CLASS in *site*"
  (dolist (obj (find-all class))
    (remhash (url-of obj) *site*)))

(defun add-to-site (obj)
  "Add OBJ to *site*"
  (let ((url (url-of obj)))
    (if (gethash url *site*)
        (error "duplicate compiled object with url ~a" url)
        (setf (gethash url *site*) obj))))

(defun delete-from-site (obj)
  "Delete OBJ from *site*"
  (let ((url (url-of obj)))
    (remhash url *site*)))

(defun write-to-disk (obj theme)
  "Write compiled HTML to disk"
  (let ((html (funcall theme obj))
        (url (namestring (url-of obj))))
    (rin-util:write-file (merge-pathnames url (merge-pathnames #p"site/" (getcwd)))
                         html)))

(defun generate-url (obj name)
  "Generate url for OBJ with NAME"
  (let* ((route (class-name (class-of obj)))
         (path (format nil "~a/~a" route name))
         (type (or (pathname-type path) "html")))
    (make-pathname :type type :defaults path)))

(defgeneric load-from-disk (class)
  (:documentation "Load all files depends on CLASS")
  (:method (class)
    (error "Unsupported class ~a~%" class)))

(defmethod load-from-disk :before (class)
  (delete-all (class-name class)))

(defgeneric save (class)
  (:documentation "Save compiled HTML to path depends on CLASS")
  (:method (class)
    (error "Unsupported class ~a~%" class)))

(defgeneric find-theme (class)
  (:documentation "Find theme template at style path depends on CLASS")
  (:method (class)
    (error "Unsupported class ~a~%" class)))

;;; site.lisp ends here
