;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:rin-asd
  (:use :cl :asdf))
(in-package :rin-asd)

(defsystem "rin"
    :version "0.1.0"
    :author "Brethland Yang <brethland@gmail.com>"
    :license "GNU General Public License v3.0"
    :description "A fast static site generator"
    :depends-on ("unix-opts")
    :components ((:module "src"
                  :components
                  ((:file "rin")))))