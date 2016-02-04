;;;; protocol.lisp --- Tests for protocol functions of the utilities.print-items system.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:print-items.test)

(in-suite utilities.print-items)

(defclass mock (print-items-mixin)
  ((a :initarg  :a
      :reader   mock-a
      :initform nil)
   (b :initarg  :b
      :reader   mock-b
      :initform nil)))

(defmethod print-items append ((object mock))
  `(,@(when (mock-a object)
        `((:a ,(mock-a object) "[~A]")))
    ,@(when (mock-b object)
        `((:b ,(mock-b object) " (~A)" ((:after :a)))))))

(test print-items-mixin.smoke
  "Smoke test for the `print-items-mixin' class."

  (mapc
   (lambda+ ((initargs expected))
     (let ((object (apply #'make-instance 'mock initargs)))
       (is (search expected (princ-to-string object)))))

   '((()          "MOCK ")
     ((:a 1)      "MOCK [1]")
     ((:b 2)      "MOCK  (2)")
     ((:a 1 :b 2) "MOCK [1] (2)")
     ((:a 2 :b 1) "MOCK [2] (1)"))))

(test format-print-items.smoke
  "Smoke test for the `format-print-items' function."

  (mapc
   (lambda+ ((input expected))
     (is (string= expected
                  (with-output-to-string (stream)
                    (format-print-items stream input)))))

   '((((:foo 1))                               "1")

     (((:foo 1) (:bar 2 nil ((:after  :foo)))) "12")
     (((:foo 1) (:bar 2 nil ((:before :foo)))) "21"))))
