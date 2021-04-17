(in-package :rin)
(defpackage :rin-util
  (:use :cl)
  (:export
   :get-cwd
   :contents
   :trim-till-newline
   :funcall-recursive
   :escape-for-xml
   :escape-for-url
   :string-to-keyword))

(in-package :rin-util)

(defun get-cwd ()
  "Get the current directory pathname in an implementation-portable way"
  (let ((dir (truename ".")))
    (if (stringp dir)
        (parse-namestring dir)
        dir)))

(defun funcall-recursive (v)
  (if (functionp v)
    (funcall-recursive (funcall v))
    v))

(defun contents (pathname)
  (with-open-file (in pathname :direction :input)
    (let* ((file-length (file-length in))
           (seq (make-string file-length))
           (pos (read-sequence seq in)))
      (if (< pos file-length)
          (subseq seq 0 pos)
          seq))))

(defun trim-till-newline (string)
  (remove #\Newline (string-right-trim '(#\Space #\Tab) string)
          :from-end t
          :count 1))

(defun escape-for-xml (string)
  "Escape special character in html"
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#39;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))

(defun escape-for-url (string)
  "Escape special character in url"
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            if (char= char #\Space)
              do (write-char #\+ out)
            else if (find char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-.")
                   do (write-char char out)
            else
              do (format out "%~2,'0x" (char-code char))))))

(defun string-to-keyword (string)
  (nth-value 0 (intern string :keyword)))

;;; util.lisp ends here
