(defun kojun-board ()
  '((5 0 2 0 2 0 3 1 3 1)
    (0 4 0 1 0 5 0 5 0 4)
    (7 5 1 7 0 0 3 1 3 0)
    (0 4 0 0 0 0 0 0 0 3)
    (2 0 3 4 0 2 0 0 4 0)
    (5 0 2 0 6 0 0 0 0 0)
    (0 1 3 0 1 0 0 4 0 3)
    (6 7 0 3 0 1 4 0 0 1)
    (4 0 3 0 4 0 0 0 0 3)
    (0 1 0 2 0 6 2 0 2 1)))

(defun kojun-regions ()
  '((01 02 02 02 03 03 03 03 04 04)
    (01 01 01 02 05 05 06 06 04 06)
    (07 07 01 05 05 08 09 06 06 06)
    (07 07 05 05 10 08 09 09 09 11)
    (07 07 07 05 10 10 12 11 11 11)
    (13 13 14 14 14 10 15 15 16 16)
    (13 13 13 14 14 18 19 20 16 16)
    (17 17 13 14 18 18 19 20 21 21)
    (17 17 22 22 22 22 19 20 20 23)
    (17 17 17 22 22 22 19 19 23 23)))

(defun print-kojun-board (board)
  (dolist (row board)
    (dolist (cell row)
      (format t "~2d " cell))
    (format t "~%")))

(defun print-region-dict (region-dict)
  (maphash (lambda (key value)
             (format t "Region ~a: ~a~%" key value))
           region-dict))

(defun create-region-dict (regions)
  (let ((region-dict (make-hash-table)))
    (loop for i from 0 to (1- (length regions)) do
          (loop for j from 0 to (1- (length (nth i regions))) do
                (let ((region (nth j (nth i regions))))
                  (push (list i j) (gethash region region-dict)))))
    region-dict))

(defun change-board-value (board row col value)
  (setf (nth col (nth row board)) value))

(defun find-empty-cell (board)
    (loop for i from 0 to (1- (length board)) do
        (loop for j from 0 to (1- (length board)) do
         (if (= (nth j (nth i board)) 0)
            (return-from find-empty-cell (list i j)))))
    nil)

(defun find-region (i j region-dict)
  (loop named outer for k being the hash-keys in region-dict using (hash-value v) do
        (loop for item in v do
              (destructuring-bind (row col) item
                (when (and (= i row) (= j col))
                    (return-from find-region k)))))
  nil) ; Return nil if no region is found

(defun get-region-cells (region regions-dict)
    (gethash region regions-dict)
)

(defun get-cell-value (board row col)
    (nth col (nth row board))
)

(defun check-if-number-exist-in-region (board region-cells empty-cell-row empty-cell-col num)
  (loop for cell in region-cells do
        (destructuring-bind (row col) cell
          (let ((cell-value (get-cell-value board row col)))
            (if (= cell-value num)
                (return-from check-if-number-exist-in-region t))
            )))
                nil)

(defun check-higher-vertical-number (board regions x y num empty-cell-region)
  (loop for row from 0 below x do
        (let* ((region-cell (find-region row y regions)))
          (when (and region-cell
                     (eql region-cell empty-cell-region)
                     (/= (get-cell-value board row y) 0)
                     (< (get-cell-value board row y) num))
            (return-from check-higher-vertical-number t))))
    )

(defun check-adjacencies (board x y num)
  (let ((board-size (length board)))
    ;; Check Above
    (when (and (>= (- x 1) 0)
               (< (- x 1) board-size)
               (>= y 0)
               (< y board-size)
               (= (get-cell-value board (- x 1) y) num))
      (return-from check-adjacencies t))
    
    ;; Check Below
    (when (and (>= (+ x 1) 0)
               (< (+ x 1) board-size)
               (>= y 0)
               (< y board-size)
               (= (get-cell-value board (+ x 1) y) num))
      (return-from check-adjacencies t))

    ;; Check Left
    (when (and (>= x 0)
               (< x board-size)
               (>= (- y 1) 0)
               (< (- y 1) board-size)
               (= (get-cell-value board x (- y 1)) num))
      (return-from check-adjacencies t))

    ;; Check Right
    (when (and (>= x 0)
               (< x board-size)
               (>= (+ y 1) 0)
               (< (+ y 1) board-size)
               (= (get-cell-value board x (+ y 1)) num))
      (return-from check-adjacencies t))

    ;; If none of the checks returned nil, return t
    ))

(defun is-valid (board regions-dict empty-cell-row empty-cell-col num)
    (let* ((region (find-region empty-cell-row empty-cell-col regions-dict))
       (region-cells (when region (get-region-cells region regions-dict))))
        (if (> num (length region-cells))
            (return-from is-valid nil))
        (if (check-if-number-exist-in-region board region-cells empty-cell-row empty-cell-col num)
            (return-from is-valid nil))
        (if (check-higher-vertical-number board regions-dict empty-cell-row empty-cell-col num region)
            (return-from is-valid nil))
        (if (check-adjacencies board empty-cell-row empty-cell-col num)
            (return-from is-valid nil))
            )
    t)



(defun solve (board regions)
    (let ((empty-cell (find-empty-cell board)))
    (if (not empty-cell)
        (return-from solve t))

    (destructuring-bind(i j) empty-cell
        (loop for num from 1 to (1- (length board)) do
            (if (is-valid board regions i j num)
                ; se for válido
                (progn
                    (change-board-value board i j num)
                    (if (solve board regions)
                        (return-from solve t)
                    )
                    (change-board-value board i j 0))))
        (return-from solve nil)
    )))


(defun main ()
  (let ((board (kojun-board)))
    (let ((regions (kojun-regions))
          (region-dict))
      (setq region-dict (create-region-dict regions))
      (if (solve board region-dict)
          (print-kojun-board board)))
    ))

(main)
