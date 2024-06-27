(require "Definitions" "./Source/Definitions.lisp")
(require "Printer" "./Source/Printer.lisp")
(require "Solver" "./Source/Solver.lisp")

; Arquivo que iniciará o resolvedor do Kojun

(defun main ()
  "Função principal que iniciará o resolvedor do Kojun."
  (let ((board (Definitions:kojun-board)))
    (let ((regions (Definitions:kojun-regions))
          (region-dict))
      (setq region-dict (Definitions:create-region-dict regions))
      (if (Solver:solve board region-dict)
          (Printer:print-kojun-board board)))
    ))

(main)
