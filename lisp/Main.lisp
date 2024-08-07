(require "Definitions" "./Source/Definitions.lisp")
(require "Printer" "./Source/Printer.lisp")
(require "Solver" "./Source/Solver.lisp")

; Arquivo que iniciará o resolvedor do Kojun

(defun main ()
  "Função principal que iniciará o resolvedor do Kojun."
  (format t "Kojun 17x17 ~%")
  (let ((board (Definitions:kojun-board-17x17)))
    (let ((regions (Definitions:kojun-regions-17x17))
          (region-dict))
      (setq region-dict (Definitions:create-region-dict regions))
      (if (Solver:solve board region-dict)
          (Printer:print-kojun-board board)))
    )
    (format t "~%")

    (format t "Kojun 10x10 ~%")
    (let ((board (Definitions:kojun-board-10x10)))
    (let ((regions (Definitions:kojun-regions-10x10))
          (region-dict))
      (setq region-dict (Definitions:create-region-dict regions))
      (if (Solver:solve board region-dict)
          (Printer:print-kojun-board board)))
    )
    (format t "~%")

    (format t "Kojun 6x6 ~%")
    (let ((board (Definitions:kojun-board-6x6)))
    (let ((regions (Definitions:kojun-regions-6x6))
          (region-dict))
      (setq region-dict (Definitions:create-region-dict regions))
      (if (Solver:solve board region-dict)
          (Printer:print-kojun-board board)))
    )
    (format t "~%")
    )

(main)
