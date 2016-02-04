;;;; package.lisp --- Package definition unit tests of the utilities.print-items system.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:print-items.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:fiveam

   #:print-items)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the utilities.print-items
system."))

(cl:in-package #:print-items.test)

(def-suite utilities.print-items)

(defun run-tests ()
  (run! 'utilities.print-items))
