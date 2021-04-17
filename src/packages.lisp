(defpackage :rin
  (:use :cl :uiop)
  (:import-from :cl-ppcre #:split)
  (:import-from :alexandria #:make-keyword)
  (:export #:main
           #:init
           #:help
           #:new))

;;; packages.lisp ends here
