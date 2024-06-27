;; Arquivo que contém funções relacionadas ao algoritmo do resolvedor do Kojun

(defpackage :Solver
  (:use :common-lisp)
  (:export  :check-if-number-exist-in-region
            :check-cell-above-same-region
            :check-cell-below-same-region
            :is-valid
            :solve
  ))

(in-package :Solver)

(require "Utils" "./Source/Utils.lisp")

(defun check-if-number-exist-in-region (board region-cells num)
    "Função que checa se o número do parâmetro já existe na região."
  (loop for cell in region-cells do
        (destructuring-bind (row col) cell
          (let ((cell-value (Utils:get-cell-value board row col)))
            (if (= cell-value num)
                (return-from check-if-number-exist-in-region t))
            )))
                nil)

(defun check-cell-above-same-region (board regions empty-cell-row empty-cell-column num)
    "Função que recebe os índices de uma célula vazia e um número, e checa se a célula acima
    é da mesma região e, caso afirmativo, checa se é maior.
    "

  (when (and (>= empty-cell-row 1)
             (/= (Utils:get-cell-value board (1- empty-cell-row) empty-cell-column) 0))
    (let* ((above-region (Utils:find-region (1- empty-cell-row) empty-cell-column regions))
           (current-region (Utils:find-region empty-cell-row empty-cell-column regions)))
      (when (and above-region
                 current-region
                 (eql above-region current-region)
                 (<= (Utils:get-cell-value board (1- empty-cell-row) empty-cell-column) num))
        (return-from check-cell-above-same-region t)))))

(defun check-cell-below-same-region (board regions empty-cell-row empty-cell-column num)
    "Função que recebe os índices de uma célula vazia e um número, e checa se a célula abaixo
    é da mesma região e, caso afirmativo, checa se é menor.
    "
  (when (and (< (1+ empty-cell-row) (length board))
             (/= (Utils:get-cell-value board (1+ empty-cell-row) empty-cell-column) 0))
    (let* ((below-region (Utils:find-region (1+ empty-cell-row) empty-cell-column regions))
           (current-region (Utils:find-region empty-cell-row empty-cell-column regions)))
      (when (and below-region
                 current-region
                 (eql below-region current-region)
                 (>= (Utils:get-cell-value board (1+ empty-cell-row) empty-cell-column) num))
        (return-from check-cell-below-same-region t)))))

(defun check-adjacencies (board empty-cell-row empty-cell-col num)
"Função que recebe os índices de uma célula vazia e um número a ser colocado nela, e checa
se o número é válido, ou seja, se não ná nenhuma célula ortogonalmente adjacente que tenha o
mesmo número.
"
  (let ((board-size (length board)))
    ; Checa célula acima
    (when (and (>= (- empty-cell-row 1) 0)
               (< (- empty-cell-row 1) board-size)
               (>= empty-cell-col 0)
               (< empty-cell-col board-size)
               (= (Utils:get-cell-value board (- empty-cell-row 1) empty-cell-col) num))
      (return-from check-adjacencies t))
    
    ; Checa célula abaixo
    (when (and (>= (+ empty-cell-row 1) 0)
               (< (+ empty-cell-row 1) board-size)
               (>= empty-cell-col 0)
               (< empty-cell-col board-size)
               (= (Utils:get-cell-value board (+ empty-cell-row 1) empty-cell-col) num))
      (return-from check-adjacencies t))

    ; Checa célula à esquerda
    (when (and (>= empty-cell-row 0)
               (< empty-cell-row board-size)
               (>= (- empty-cell-col 1) 0)
               (< (- empty-cell-col 1) board-size)
               (= (Utils:get-cell-value board empty-cell-row (- empty-cell-col 1)) num))
      (return-from check-adjacencies t))

    ; Checa célula à direita
    (when (and (>= empty-cell-row 0)
               (< empty-cell-row board-size)
               (>= (+ empty-cell-col 1) 0)
               (< (+ empty-cell-col 1) board-size)
               (= (Utils:get-cell-value board empty-cell-row (+ empty-cell-col 1)) num))
      (return-from check-adjacencies t))
    ))

(defun is-valid (board regions-dict empty-cell-row empty-cell-col num)
"Funcão que recebe os índices de uma célula vazia e um número, e checa se o número é valido, ou seja, se respeita
todas as regras do puzzle."
    (let* ((region (Utils:find-region empty-cell-row empty-cell-col regions-dict))
       (region-cells (when region (Utils:get-region-cells region regions-dict))))
        (if (> num (length region-cells))
            (return-from is-valid nil))
        (if (check-if-number-exist-in-region board region-cells num)
            (return-from is-valid nil))
        (if (check-cell-above-same-region board regions-dict empty-cell-row empty-cell-col num)
            (return-from is-valid nil))
        (if (check-cell-below-same-region board regions-dict empty-cell-row empty-cell-col num)
            (return-from is-valid nil))
        (if (check-adjacencies board empty-cell-row empty-cell-col num)
            (return-from is-valid nil))
            )
    t)



(defun solve (board regions)
    "Função recursiva que resolve o Kojun."
    (let ((empty-cell (Utils:find-empty-cell board)))
    (if (not empty-cell)
        (return-from solve t))

    (destructuring-bind(empty-cell-row empty-cell-col) empty-cell
        (loop for num from 1 to (1- (length board)) do
            (if (is-valid board regions empty-cell-row empty-cell-col num)
                ; se for válido
                (progn
                    (Utils:change-board-value board empty-cell-row empty-cell-col num)
                    (if (solve board regions)
                        (return-from solve t)
                    )
                    (Utils:change-board-value board empty-cell-row empty-cell-col 0))))
        (return-from solve nil)
    )))