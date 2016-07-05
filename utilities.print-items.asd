;;;; utilities.print-items.asd --- System definition for utilities.print-items.
;;;;
;;;; Copyright (C) 2010-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :utilities.print-items
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version.sexp")
  :license     "LLGPLv3" ; see COPYING file for details
  :description "A protocol for flexible and composable printing,
primarily unreadable printing"
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
  :version     (:read-file-form "version.sexp")
  :license     "LLGPLv3" ; see COPYING file for details
  :description "Unit tests for the utilities.print-items system."
  :depends-on  ((:version :utilities.print-items (:read-file-form "version.sexp"))
                (:version :fiveam                "1.1"))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :utilities.print-items-test))))
  (uiop:symbol-call '#:print-items.test '#:run-tests))
