(defpackage :rin-util
  (:use :cl)
  (:import-from :alexandria #:make-keyword)
  (:export #:contents
           #:slurp
           #:trim-till-newline
           #:escape-for-xml
           #:escape-for-url
           #:parse-initarg
           #:parse-metadata
           #:do-files
           #:write-file))

(in-package :rin-util)

(defun contents (pathname)
  (with-open-file (in pathname :direction :input)
    (let* ((file-length (file-length in))
           (seq (make-string file-length))
           (pos (read-sequence seq in)))
      (if (< pos file-length)
          (subseq seq 0 pos)
          seq))))

(defun slurp (stream)
  "Get remained string in STREAM"
  (let ((seq (make-string (- (file-length stream) (file-position stream)))))
    (read-sequence seq stream)
    (remove #\Nul seq)))

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

(defun parse-initarg (string)
  "Parse a initarg name/value pair from STRING"
  (let ((name (string-upcase (subseq string 0 (position #\: string))))
        (match (nth-value 1 (ppcre:scan-to-strings "[a-zA-Z]+:\\s+(.*)" string))))
    (when match
      (list (make-keyword name) (aref match 0)))))

(defun parse-metadata (stream)
  "Given a STREAM, parse metadata from it"
  (flet ((get-next-line (input)
           (string-trim '(#\Space #\Return #\Newline #\Tab) (read-line input nil))))
    (unless (string= (get-next-line stream) "---")
      (error "The file ~a lacks metadata" (file-namestring stream)))
    (loop for line = (get-next-line stream)
       until (string= line "---")
          appending (parse-initarg line))))

(defmacro do-files ((var path) &body body)
  "Iterate files in PATH and run BODY"
  `(cl-fad:walk-directory ,path (lambda (,var) ,@body)
                          :follow-symlinks nil))

(defun write-file (path file)
  (ensure-directories-exist path)
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :external-format :utf-8)
    (write file :stream out :escape nil)))

;;; util.lisp ends here
