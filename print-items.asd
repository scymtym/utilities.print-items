;;;; print-items.asd --- System definition for print-items.
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:print-items-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:print-items-system)


;;; Version stuff
;;

(defconstant +version-major+ 0
  "Major component of version number.")

(defconstant +version-minor+ 1
  "Minor component of version number.")

(defconstant +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))


;;; System definition
;;

(defsystem :print-items
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "This system provides some generic condition classes in
conjunction with support functions and macros."
  :depends-on  (:alexandria)
  :components  ((:module     "src"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "protocol"))))

  :in-order-to ((test-op (test-op :print-items-test))))

(defsystem :print-items-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Unit tests for the cl-print-items system."
  :depends-on  (:cl-print-items
		:lift)
  :components  ((:module     "test"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "conditions")
			      (:file       "macros")))))

(defmethod perform ((op test-op) (component (eql (find-system :print-items-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))
