(in-package :rin)

(defclass feed ()
  ((url     :initarg :url     :reader feed-url)
   (content :initarg :content :reader feed-content)))

(defmethod url-of ((obj feed))
  (feed-url obj))

(defmethod initialize-instance :after ((object feed) &key slug)
  (with-slots (url) object
    (setf url (generate-url object slug))))

(defmethod load-from-disk ((class (eql (find-class 'feed))))
  (let* ((content (by-date (find-all 'post)))
         (feed (make-instance 'feed :content (rin-util:take-up-to 10 content)
                                   :slug (format nil "~a" 'feed.xml))))
    (add-to-site feed)))

(defmethod save ((class (eql (find-class 'feed))))
  (dolist (feed (find-all 'feed))
    (write-to-disk feed (find-theme class))))

(defmethod find-theme ((class (eql (find-class 'feed))))
  (let ((path (merge-pathnames #p"style/feed.html" (getcwd))))
    (if (probe-file path)
        (progn
          (rin-template:add-template "feed" path)
          (lambda (obj)
            (let ((env (list :title (title *site-config*)
                             :domain (domain *site-config*)
                             :author (author *site-config*)
                             :date (format-rfc1123-timestring nil (local-time:now))
                             :content (feed-content obj))))
              (rin-template:execute "feed" :env env))))
        (error "Missing template 'style/feed.html' for feeds"))))
