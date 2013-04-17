;;;; protocol.lisp --- Protocol functions of the utilities.print-items system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:print-items)

;;; Print items protocol

(defgeneric print-items (object)
  (:method-combination append)
  (:documentation
   "Return a list of items that should appear in the printed
representation of OBJECT.

Each method should return a list of items of the form

  (KEY VALUE [FORMAT [CONSTRAINT*]]

where

  KEY        ::= any Lisp object
  VALUE      ::= any Lisp object
  FORMAT     ::= a format string (Default is \"~A\")

  CONSTRAINT ::= (:before | :after) KEY

When multiple items have `eql' KEYs, items appearing closer to the
beginning of the item list take precedence. This mechanism can be used
to replace print items produced by superclasses in subclasses."))

(defmethod print-items append ((object t))
  "Default behavior is to not return any print items for OBJECT."
  nil)

;;; Print items mixin

(defclass print-items-mixin ()
  ()
  (:documentation
   "This mixin class adds printing via `print-items' to classes."))

(defmethod print-object ((object print-items-mixin) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "~A" (class-name (class-of object)))
    (format-print-items stream (print-items object))))

;;; Utility functions

(defun format-print-items (stream items &optional colon? at?)
  "Print ITEMS onto STREAM.

ITEMS is a list of items of the form

  (KEY VALUE [FORMAT [CONSTRAINT*]]

where

  KEY        ::= any Lisp object
  VALUE      ::= any Lisp object
  FORMAT     ::= a format string (Default is \"~A\")

  CONSTRAINT ::= (:before | :after) KEY"
  (declare (ignore colon? at?))
  (mapc (lambda+ ((&ign value &optional format &ign))
          (format stream (or format "~A") value))
        (sort-with-partial-order
         (remove-duplicates items
                            :key      #'first
                            :from-end t)
         #'item-<)))
