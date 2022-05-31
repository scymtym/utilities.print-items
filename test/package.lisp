;;;; package.lisp --- Package definition unit tests of the utilities.print-items system.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:utilities.print-items.test
  (:use
   #:cl
   #:alexandria

   #:fiveam

   #:utilities.print-items)

  (:import-from #:utilities.print-items
   #:parse-item

   #:cycle-error
   #:sort-with-partial-order)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the utilities.print-items
system."))

(cl:in-package #:utilities.print-items.test)

(def-suite utilities.print-items)

(defun run-tests ()
  (run! 'utilities.print-items))
