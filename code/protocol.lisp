;;;; protocol.lisp --- Protocol functions of the utilities.print-items system.
;;;;
;;;; Copyright (C) 2011-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.print-items)

;;; Print items protocol

(defgeneric print-items (object)
  (:method-combination append)
  (:documentation
   "Return a list of items that should appear in the printed
    representation of OBJECT.

    Each method should return a list of items of the form

      ITEM              ::= (KEY-AND-OPTIONS FORMAT-CONTROL ARGUMENT*)

      KEY-AND-OPTIONS   ::= KEY
                            | (KEY OPTION*)
      KEY               ::= any Lisp object
      OPTION            ::= CONSTRAINT
      CONSTRAINT        ::= ((:before | :after) KEY)

      FORMAT-CONTROL    ::= `nil'
                            | a format control string or a formatter function
      ARGUMENT          ::= any Lisp object

    When multiple items have `eql' KEYs, items appearing closer to the
    beginning of the item list take precedence. This mechanism can be
    used by subclasses to replace print items produced by
    superclasses.

    When FORMAT-CONTROL is `nil' the whole item is ignored. This
    mechanism can be used by subclasses to disable print items
    produced by superclasses."))

(defgeneric effective-print-items (object)
  (:documentation
   "Return a list of items like `print-items', but filtered and sorted.

    Filtering removes all but the first occurrence of multiple items
    using the same key.

    Sorting arranges the filtered items according to their specified
    constraints."))

;;; Default behavior

(defmethod print-items append ((object t))
  ;; Default behavior is to not return any print items for OBJECT.
  '())

(defmethod effective-print-items ((object t))
  (let* ((raw    (print-items object))
         (unique (remove-duplicates raw :key #'first :from-end t)))
    (sort-with-partial-order unique #'item-<)))

;;; Formatting functions

(defun format-item (stream item &optional colon? at?)
  (declare (ignore colon? at?))
  (destructure-item (nil enabled? format arguments) item
    (when enabled?
      (format stream "~?" format arguments))))

(defun format-items (stream items &optional colon? at?)
  "Print ITEMS onto STREAM.

   ITEMS is a list of items of the form

     (KEY-AND-OPTIONS FORMAT-CONTROL ARGUMENT*)

   where

     KEY-AND-OPTIONS ::= KEY
                         | (KEY OPTION*)
     KEY             ::= any Lisp object
     OPTION          ::= CONSTRAINT
     CONSTRAINT      ::= ((:before | :after) KEY)

     FORMAT-CONTROL  ::= `nil'
                         | a format control string or a formatter function
     ARGUMENT        ::= any Lisp object"
  (declare (ignore colon? at?))
  (mapc (curry #'format-item stream)
        ;; TODO we might remove this later and expect the client to
        ;; pass in the effective list of items
        (sort-with-partial-order
         (remove-duplicates items :key #'first :from-end t)
         #'item-<)))

(defun format-print-items (stream items &optional colon? at?)
  "Alias for `format-items'."
  (format-items stream items colon? at?))

;;; Print items mixin

(defclass print-items-mixin ()
  ()
  (:documentation
   "This mixin class adds printing via `print-items' to classes."))

(defmethod print-object ((object print-items-mixin) stream)
  (cond (*print-readably*
         (call-next-method))
        (t
         (print-unreadable-object (object stream :identity t)
           (format stream "~A~@[ ~/print-items:format-items/~]"
                   (class-name (class-of object))
                   (effective-print-items object))))))
