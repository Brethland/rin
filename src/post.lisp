(in-package :rin)

(defclass tag ()
  ((url :initarg :url)
   (name :initarg :name :reader tag-name)))

(defmethod initialize-instance :after ((object tag) &key)
  (with-slots (url name) object
    (setf url (generate-url object (rin-util:escape-for-url name)))))

(defun make-tag (string)
  (let ((trimmed (string-trim " " string)))
    (make-instance 'tag :name trimmed)))

(defclass post ()
  ((title :initarg :title :reader post-title)
   (author :initarg :author :reader post-author)
   (type :initarg :type :reader post-type)
   (excerpt :initarg :excerpt :reader post-excerpt)
   (url :initarg :url :reader post-url)
   (date :initarg :date :reader post-date)
   (file :initarg :file :reader post-file)
   (tags :initarg :tags :reader post-tags)
   (content :initarg :content :reader post-content))
  (:default-initargs :excerpt nil :tags nil))

(defmethod initialize-instance :after ((object post) &key)
  (with-slots (url file author excerpt type content tags) object
    (let* ((doc-type (make-keyword (string-upcase type)))
           (rendered (render content doc-type)))
      (setf url (generate-url object (rin-util:escape-for-url file))
            type doc-type
            excerpt (or excerpt (first (split (excerpt-sep *site-config*)
                                              rendered
                                              :limit 2)))
            content rendered
            author (or author (author *site-config*))))
    (when (stringp tags)
      (setf tags (mapcar #'make-tag (split "," tags))))))

(defmethod url-of ((obj post))
  (post-url obj))

(defun parse (file)
  "Parse metadata and content from FILE"
  (with-open-file (in file :external-format :utf-8)
    (let ((metadata (rin-util:parse-metadata in))
          (content (rin-util:slurp in))
          (filepath (enough-namestring file (getcwd))))
      (append metadata (list :content content :file filepath)))))

(defgeneric render (text type)
  (:documentation "Render given TEXT to HTML"))

(defmethod render (text (type (eql :html)))
  "If html, keep original"
  text)

(defmethod render (text (type (eql :markdown)))
  "If markdown, use 3bmd"
  (let ((3bmd-code-blocks:*code-blocks* t))
    (with-output-to-string (out)
      (3bmd:parse-string-and-print-to-stream text out))))

(defmethod render (text (type (eql :md)))
  "If markdown, use 3bmd"
  (let ((3bmd-code-blocks:*code-blocks* t))
    (with-output-to-string (out)
      (3bmd:parse-string-and-print-to-stream text out))))

(defmethod render (text (type (eql :org)))
  "If org, ..."
  text)

(defmethod load-from-disk ((class (eql (find-class 'post))))
  (let ((path (merge-pathnames #p"post/" (getcwd))))
    (rin-util:do-files (file path)
      (let ((post (apply 'make-instance (class-name class) (parse file))))
        (add-to-site post)))))

(defun by-date (post)
  "Sort CONTENT in reverse chronological order."
  (sort post #'string> :key #'post-date))

(defmethod save ((class (eql (find-class 'post))))
  (loop for (next post prev) on (append '(nil) (by-date (find-all 'post)))
    while post do (write-to-disk post nil :prev prev :next next)))

;;; post.lisp ends here
