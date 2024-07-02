(defpackage :Printer
  (:use :common-lisp)
  (:export :print-kojun-board
            :print-kojun-regions
  ))

(in-package :Printer)

(defun print-kojun-board (board)
    "Função que printa o estado atual do board."
  (dolist (row board)
    (dolist (cell row)
      (format t "~2d " cell))
    (format t "~%")))

(defun print-region-dict (region-dict)
  (maphash (lambda (key value)
             (format t "Region ~a: ~a~%" key value))
           region-dict))