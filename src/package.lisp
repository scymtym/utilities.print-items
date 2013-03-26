;;;; package.lisp --- Package definition for the print-items system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:print-items
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate)

  ;; print items protocol
  (:export
   #:print-items)

  ;; `print-items-mixin' mixin class
  (:export
   #:print-items-mixin)

  ;; Utility functions
  (:export
   #:format-print-items)

  (:documentation
   "This package contains the print-items system for composable
printing. The interface consists of

* `print-items'                     [generic function]
  Methods extend the printed representation of an object in a
  composable manner.

* `print-items-mixin'               [class]
  Can be used to make subclasses the print items mechanism when passed
  to `print-object'.

* `format-print-items'              [function]
  Utility function for formatting print items onto a stream."))
