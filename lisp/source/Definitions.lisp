(defpackage :Definitions
  (:use :common-lisp)
  (:export  :kojun-board
            :kojun-regions
            :create-region-dict
  ))

(in-package :Definitions)

(defun kojun-board ()
  "Valores iniciais do Kojun."
  '((0 3 0 0 0 0 5 1 0 3 1 0 7 3 0 4 2)
    (6 0 0 0 0 3 0 0 0 0 2 5 0 0 0 3 4)
    (5 0 0 1 5 0 2 0 0 5 0 0 4 1 0 0 0)
    (0 0 0 0 0 0 0 0 0 2 7 0 6 0 4 0 6)
    (6 0 0 4 3 0 0 2 0 0 0 0 2 1 0 4 0)
    (0 0 0 0 0 5 0 0 3 0 4 0 5 0 5 0 2)
    (1 0 0 0 0 3 0 0 0 5 0 3 0 0 4 0 0)
    (0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 2)
    (0 0 5 0 0 0 0 0 0 0 0 0 0 6 4 0 0)
    (0 4 0 0 4 0 3 0 3 0 5 0 2 0 2 0 0)
    (5 0 2 0 3 5 0 6 0 0 0 0 0 5 0 3 0)
    (0 0 4 2 0 0 7 0 0 7 0 0 3 0 6 5 0)
    (3 0 0 1 4 0 2 4 0 0 2 0 0 0 5 0 0)
    (0 1 0 6 0 2 0 0 0 3 0 5 0 0 3 6 2)
    (0 0 4 0 5 6 2 0 0 0 3 0 0 1 0 4 7)
    (2 0 2 0 4 0 0 0 1 0 0 1 0 4 0 0 0)
    (1 7 6 5 0 4 1 7 2 3 1 2 3 0 2 0 0)))

(defun kojun-regions ()
  "Regiões do Kojun"
  '((01 02 02 03 04 04 05 05 05 05 06 06 06 06 06 06 06) 
    (02 02 02 03 03 04 05 07 07 08 08 09 10 10 10 10 11) 
    (02 02 13 03 14 04 05 05 07 07 07 09 10 10 10 11 11) 
    (12 12 13 13 14 15 16 16 07 07 09 09 18 18 18 11 19) 
    (20 21 22 13 14 15 24 16 16 09 09 09 18 18 18 19 19) 
    (20 22 22 13 14 25 24 24 26 27 27 27 27 28 28 19 19) 
    (20 20 23 14 14 25 25 26 26 26 29 27 27 28 28 30 19) 
    (31 20 23 32 25 25 25 35 36 26 29 38 39 39 28 30 30) 
    (31 20 32 32 33 33 34 35 36 37 37 38 40 40 30 30 41) 
    (31 42 32 32 34 34 34 35 36 45 45 38 38 40 40 30 41) 
    (42 42 42 43 43 43 34 44 36 36 45 46 46 47 40 40 41) 
    (49 42 43 43 43 44 44 44 36 45 45 46 47 47 41 41 41) 
    (49 49 50 51 51 44 44 44 54 45 45 46 47 47 48 48 48) 
    (49 49 50 50 51 51 52 53 54 54 55 55 56 56 48 57 48) 
    (59 59 50 50 52 52 52 53 53 53 55 55 56 56 57 57 57) 
    (59 64 50 50 52 52 60 61 61 62 62 55 56 63 63 57 58) 
    (59 59 59 59 60 60 60 60 60 60 62 62 62 63 63 57 57) ))

(defun create-region-dict (regions)
  "Cria a hash table das regiões."
  (let ((region-dict (make-hash-table)))
    (loop for i from 0 to (1- (length regions)) do
          (loop for j from 0 to (1- (length (nth i regions))) do
                (let ((region (nth j (nth i regions))))
                  (push (list i j) (gethash region region-dict)))))
    region-dict))