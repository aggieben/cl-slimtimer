
(in-package :slimtimer)
(export '(time-entry-task task-name task-hours task-last))

(require :s-xml)

(defmacro with-xml (&body body)
  `(with-output-to-string (ss)
     (format ss "<?xml version=1.0\" encoding=\"utf-8\"?>")
     (cl-who:with-html-output (ss)
       ,@body)))

(defun mkstr (&rest args)
  (with-output-to-string (ss)
    (dolist (a args) (princ a ss))))

(defmacro time-entry-task (entry)
  `(fifth ,entry))

(defmacro task-name (task)
  `(cadr (second (cdr ,task))))

(defmacro task-name (task)
  `(cadr (nth 6 (cdr ,task))))

(defmacro task-hours (task)
  `(cadr (nth 8 (cdr ,task))))

(defmacro task-last (task)
  `(cadr (nth 2 (cdr ,task))))