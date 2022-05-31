;;;; package.lisp --- Package definition for the utilities.print-items system.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:utilities.print-items
  (:nicknames
   #:print-items)

  (:use
   #:cl
   #:alexandria)

  ;; Print items protocol
  (:export
   #:print-items)

  ;; Formatting functions
  (:export
   #:format-print-items)

  ;; `print-items-mixin' mixin class
  (:export
   #:print-items-mixin)

  (:documentation
   "Utilities for composable printing.

    The interface consists of

    * `print-items'                     [generic function]

      Methods extend the printed representation of an object in a
      composable manner.

    * `print-items-mixin'               [class]

      Can be used to make subclasses the print items mechanism when
      passed to `print-object'.

    * `format-print-items'              [function]

      Utility function for formatting print items onto a stream."))
