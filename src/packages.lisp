(defpackage :rin
  (:use :cl :uiop)
  (:import-from :cl-ppcre #:split)
  (:import-from :alexandria #:make-keyword)
  (:import-from :local-time #:format-rfc1123-timestring)
  (:export #:main
           #:init
           #:help
           #:new))

;;; packages.lisp ends here
