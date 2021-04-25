(in-package :rin)

(defvar *cfg-msg*
  ";;; This is config file for your site.~2%
(:domain \"example.com\"
 :title \"blog\"
 :author \"joe\"
 :doc-type \"markdown\")")

(defvar *new-post-msg*
  "---
title: ~a
author: ~a
type: ~a
date: ~a-~2,,,'0@a-~2,,,'0@a ~2,,,'0@a:~2,,,'0@a:~2,,,'0@a
tags:
---

<!-- Your post starts here -->")

(defun read-config ()
  "Read config from current dir without load it"
  (let ((path (merge-pathnames "config" (getcwd))))
    (if (probe-file path)
        (with-open-file (in path)
          (read in))
        nil)))

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
           :arg-parser #'identity)
    (:name :new
           :description "create a new post"
           :short #\n
           :long "new"
           :arg-parser #'identity)
    (:name :generate
           :description "build site under /site folder"
           :short #\g
           :long "generate")))

(defun unknown-option (condition)
  "Error handler for opts:unknown-option"
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun missing-arg (condition)
  "Error handler for opts:missing-arg"
  (format t "fatal: option ~s needs an argument!~%"
    (opts:option condition))
  (quit -1))

(defun arg-parser-failed (condition)
  "Error handler for opts:arg-parser-failed"
  (format t "fatal: cannot parse ~s as argument of ~s~%"
    (opts:raw-arg condition)
    (opts:option condition))
  (quit -1))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun help ()
  (opts:describe
   :prefix "rin, a fast static site generator"))

(defun init (dir)
  "Create a new empty project in given path"
  (let* ((user-path (getcwd))
         (path (merge-pathnames (concatenate 'string dir "/") user-path)))
    (ensure-directories-exist path)
    (ensure-directories-exist (merge-pathnames #p"post/" path))
    (ensure-directories-exist (merge-pathnames #p"static/" path))
    (ensure-directories-exist (merge-pathnames #p"style/" path))
    (with-open-file (stream (merge-pathnames #p"config" path)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream *cfg-msg*))
    (format t "Your site has been created!~%")))

(defun new (name)
  "Create a new document with current time"
  (let ((cfg (read-config)))
    (if (null cfg)
        (format *error-output* "no config file in this directory~%")
        (let ((author (getf cfg :author ""))
              (doc-type (getf cfg :doc-type "markdown")))
          (multiple-value-bind
              (second minute hour date month year day-of-week dst-p tz)
              (get-decoded-time)
              (declare (ignore day-of-week dst-p tz))
            (let* ((file-name (format nil "~a-~2,,,'0@a-~2,,,'0@a-~a" year month date name))
                   (path (merge-pathnames (make-pathname :name file-name :type doc-type) #p"post/")))
              (with-open-file (in path :direction :output :if-exists :error :if-does-not-exist :create)
                (format in *new-post-msg* name author doc-type
                        year month date hour minute second)
                (format t "Successfully create file ~a!~%" path))))))))

(defun generate ()
  "Generate HTML file to /site"
  (let ((site-path (merge-pathnames #p"site/" (getcwd))))
    (progn
      (load-config)
      (ensure-directories-exist site-path)
      (sync-static site-path)
      (load-from-disk (find-class 'post))
      (save (find-class 'post))
      (load-from-disk (find-class 'feed))
      (save (find-class 'feed))
      (format t "Successfully compile site!~%"))))

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
      (help)
      (quit 0))
    (when-option (options :init)
      (init it)
      (quit 0))
    (when-option (options :new)
      (new it)
      (quit 0))
    (when-option (options :generate)
      (generate)
      (quit 0))
    (when free-args
      (format t "unexpected args: ~a~2%"
        free-args))
    (format t "use -h to see help~%")
    (quit -1)))

;;; rin.lisp ends here
