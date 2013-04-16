;;;; package.lisp --- Package definition unit tests of the print-items system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:print-items.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:eos

   #:print-items)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the print-items system."))

(cl:in-package #:print-items.test)

(eos:def-suite print-items)

(defun run-tests ()
  (eos:run! 'print-items))
