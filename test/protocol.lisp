;;;; package.lisp --- Package definition unit tests of the utilities.print-items system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:print-items.test)

(in-suite utilities.print-items)

(test smoke
  (mapc
   (curry #'apply (lambda (input expected)
                    (is (string= (with-output-to-string (stream)
                                   (format-print-items stream input))
                                 expected))))

   '((((:foo 1))                               "1")

     (((:foo 1) (:bar 2 nil ((:after  :foo)))) "12")
     (((:foo 1) (:bar 2 nil ((:before :foo)))) "21"))))
