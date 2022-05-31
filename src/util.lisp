;;;; util.lisp --- Utilities used by the utilities.print-items system.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.print-items)

;;; Destructuring

(defun maybe-convert-legacy-item (item)
  (typecase item
    ((cons symbol (cons (not string) (or (cons null null))))
     (list (first item) nil))
    ((cons symbol (cons (not string) (or (cons null (cons list null))
                                         (cons string))))
     (destructuring-bind (key value format-control &optional options) item
       (list (list* key options) (or format-control "~A") value)))
    (t
     item)))

(defun parse-item (item)
  (destructuring-bind
      (key-and-options &optional format-control &rest arguments)
      (maybe-convert-legacy-item item)
    (multiple-value-bind (key options)
        (if (consp key-and-options)
            (values (first key-and-options) (rest key-and-options))
            (values key-and-options         '()))
      (multiple-value-bind (enabled? format-control arguments)
          (typecase format-control
            (null
             (values nil nil '()))
            ((or string function)
             (values t format-control arguments))
            (t
             (values t "~A" (list format-control))))
        (values key enabled? format-control arguments options)))))

(defmacro destructure-item ((key
                             &optional enabled? format-control values options)
                            item &body body)
  (let ((names   '())
        (ignored '()))
    (mapl (lambda (remainder)
            (destructuring-bind (name . rest) remainder
              (cond (name
                     (push name names))
                    ((find nil rest :test-not #'eq)
                     (let ((name (gensym)))
                       (push name names)
                       (push name ignored))))))
          (list key enabled? format-control values options))
    `(multiple-value-bind ,(reverse names) (parse-item ,item)
       (declare (ignore ,@ignored))
       ,@body)))

;;; Topological sort

(define-condition cycle-error (error)
  ((%path :initarg :path
          :reader  cycle-error-path))
  (:default-initargs
   :path (error "Missing required initarg for class ~S: ~S"
                'cycle-error :path))
  (:report
   (lambda (condition stream)
     (format stream "~@<The following elements form a cycle:~
                     ~@:_    ~{~A~^~@:_ -> ~}~@:>"
             (cycle-error-path condition)))))

(defstruct (node
             (:constructor make-node (object))
             (:predicate nil)
             (:copier nil))
  (object nil                                         :read-only t)
  (edges  '()  :type list)
  (state  :new :type (member :new :in-progress :done)))

(defun topological-sort (nodes)
  (let ((new    (coerce nodes 'list))
        (result '()))
    (labels ((visit (node)
               (case (node-state node)
                 (:in-progress
                  (list node))
                 (:new
                  (setf (node-state node) :in-progress)
                  (when-let ((cycle (some #'visit (node-edges node))))
                    (return-from visit (list* node cycle)))
                  (setf (node-state node) :done)
                  (push node result)
                  nil))))
      (loop :for node = (pop new)
            :while node
            :do (when-let ((cycle (visit node)))
                  (error 'cycle-error :path (mapcar #'node-object cycle)))))
    result))

(defun sort-with-partial-order (sequence predicate)
  (declare (type sequence sequence))
  (let* ((predicate (ensure-function predicate))
         (nodes     (map '(simple-array t 1) #'make-node sequence))
         (length    (length nodes)))
    ;; Build graph by checking PREDICATE for all pairs of distinct
    ;; elements of SEQUENCE.
    (loop :for i :below length
          :for node1   = (aref nodes i)
          :for object1 = (node-object node1)
          :do (loop :for j :from (1+ i) :below length
                    :for node2   = (aref nodes j)
                    :for object2 = (node-object node2)
                    :when (funcall predicate object1 object2)
                    :do (push node2 (node-edges node1))
                    :when (funcall predicate object2 object1)
                    :do (push node1 (node-edges node2))))
    ;; Topologically sort nodes and extract objects.
    (mapcar #'node-object (topological-sort nodes))))

;;; Comparator

(defun item-< (left right)
  "Return non-nil if the left item should be placed before the right."
  (destructure-item (key-left nil nil nil options-left) left
    (destructure-item (key-right nil nil nil options-right) right
      (flet ((satisfied? (constraint other transpose?)
               (destructuring-bind (kind target) constraint
                 (ecase kind
                   (:after  (and transpose?       (eql other target)))
                   (:before (and (not transpose?) (eql other target)))))))
        (or (some (rcurry #'satisfied? key-right nil) options-left)
            (some (rcurry #'satisfied? key-left  t)   options-right))))))
