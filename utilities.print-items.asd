;;;; utilities.print-items.asd --- System definition for utilities.print-items.
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:utilities.print-items-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:utilities.print-items-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 1
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))

;;; System definition

(defsystem :utilities.print-items
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "This system provides some generic condition classes in
conjunction with support functions and macros."
  :depends-on  (:alexandria
                (:version :let-plus "0.2")
                :iterate)
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "protocol")))

                (:static-file "README.org"))

  :in-order-to ((test-op (test-op :utilities.print-items-test))))

(defsystem :utilities.print-items-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Unit tests for the cl-print-items system."
  :depends-on  ((:version :utilities.print-items #.(version/string))
                :eos)
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :utilities.print-items-test))))
  (funcall (find-symbol "RUN-TESTS" :print-items.test)))
