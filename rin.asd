;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:rin-asd
  (:use :cl :asdf))
(in-package :rin-asd)

(defsystem "rin"
  :version "0.1.0"
  :author "Brethland Yang <brethland@gmail.com>"
  :license "GNU General Public License v3.0"
  :description "A fast static site generator"
  :depends-on (:unix-opts
               :cl-ppcre
               :uiop
               :alexandria
               :cl-fad
               :3bmd
               :3bmd-ext-code-blocks)
  :components ((:module "src"
                :components
                ((:file "rin" :depends-on ("packages"))
                 (:file "site" :depends-on ("packages"))
                 (:file "post" :depends-on ("packages"))
                 (:file "template" :depends-on ("util"))
                 (:file "packages" :depends-on ("util"))
                 (:file "util")))))

;;; rin.asd end here
