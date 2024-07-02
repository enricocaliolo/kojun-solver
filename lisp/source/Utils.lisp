(defpackage :Utils
  (:use :common-lisp)
  (:export  :create-region-dict
            :change-board-value
            :find-empty-cell
            :find-region
            :get-region-cells
            :get-cell-value
  ))

(in-package :Utils)

(defun change-board-value (board row col value)
    "Função que troca o valor no board."
  (setf (nth col (nth row board)) value))

(defun find-empty-cell (board)
    "Função que procura a primeira célula vazia do board."
    (loop for i from 0 to (1- (length board)) do
        (loop for j from 0 to (1- (length board)) do
         (if (= (nth j (nth i board)) 0)
            (return-from find-empty-cell (list i j)))))
    nil)

(defun find-region (i j region-dict)
    "Função que recebe os indíces uma célula e retorna sua região."
  (loop named outer for k being the hash-keys in region-dict using (hash-value v) do
        (loop for item in v do
              (destructuring-bind (row col) item
                (when (and (= i row) (= j col))
                    (return-from find-region k)))))
  nil)

(defun get-region-cells (region regions-dict)
    "Função que recebe o número de uma região e retorna as células da região."
    (gethash region regions-dict)
)

(defun get-cell-value (board row col)
    "Função que recebe os índices de uma célula e retorna seu valor."
    (nth col (nth row board))
)