;;;; util.lisp --- Tests for the utilities used in the utilities.print-items system.
;;;;
;;;; Copyright (C) 2013-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.print-items.test)

(in-suite utilities.print-items)

;;; Destructuring

(test parse-item.smoke
  "Smoke test for the `parse-item' function."
  (mapc (lambda (item-and-expected)
          (destructuring-bind (item expected-key expected-enabledp
                               expected-format-control expected-arguments
                               expected-options)
              item-and-expected
            (multiple-value-bind
                  (key enabledp format-control arguments options)
                (parse-item item)
              (assert (is (eq    expected-key            key)))
              (assert (is (eq    expected-enabledp       enabledp)))
              (assert (is (equal expected-format-control format-control)))
              (assert (is (equal expected-arguments      arguments)))
              (assert (is (equal expected-options        options))))))
        '(;; Current item syntax
          ((:key)                             :key nil nil   ()      ())
          ((:key nil)                         :key nil nil   ()      ())
          ((:key "foo")                       :key t   "foo" ()      ())
          ((:key 1)                           :key t   "~A"  (1)     ())
          ((:key "foo" nil)                   :key t   "foo" (nil)   ())
          ((:key "foo" 1)                     :key t   "foo" (1)     ())
          ((:key "foo" 1 2)                   :key t   "foo" (1 2)   ())
          (((:key) "foo" 1 2)                 :key t   "foo" (1 2)   ())
          (((:key (:after :baz)) "foo" 1 2)   :key t   "foo" (1 2)   ((:after :baz)))

          ;; Legacy item syntax
          ((:key 1     nil)                   :key nil nil   ()      ())
          ((:key 1     nil ((:after :baz)))   :key t   "~A"  (1)     ((:after :baz)))
          ((:key 1     "bar")                 :key t   "bar" (1)     ())
          ((:key 1     "bar" ((:after :baz))) :key t   "bar" (1)     ((:after :baz)))

          ;; Unsupported legacy item syntax
          ;; ((:key "foo" nil)                   :key nil nil   ()      ())
          ;; ((:key "foo" nil ((:after :baz)))   :key t   "~A"  ("foo") ((:after :baz)))
          ;; ((:key "foo" "bar")                 :key t   "bar" ("foo") ())
          ;; ((:key "foo" "bar" ((:after :baz))) :key t   "bar" ("foo") ((:after :baz)))
          )))

;;; Topological sort

(test sort-with-partial-order.cycle
  "Test error signaling in the `sort-with-partial-order' function."

  (signals cycle-error
    (sort-with-partial-order '(1 2 3) (constantly t))))

(test sort-with-partial-order.random
  "Random test for the `sort-with-partial-order' function."

  (fiveam:for-all ((list (gen-list)))
    (is (equal (sort (copy-list list) #'<)
               (sort-with-partial-order list #'<)))))
