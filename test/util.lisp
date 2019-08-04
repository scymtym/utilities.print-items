;;;; util.lisp --- Tests for the utilities used in the utilities.print-items system.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.print-items.test)

(in-suite utilities.print-items)

(test sort-with-partial-order.cycle
  "Test error signaling in the `sort-with-partial-order' function."

  (signals cycle-error
    (sort-with-partial-order '(1 2 3) (constantly t))))

(test sort-with-partial-order.random
  "Random test for the `sort-with-partial-order' function."

  (fiveam:for-all ((list (gen-list)))
    (is (equal (sort (copy-list list) #'<)
               (sort-with-partial-order list #'<)))))
