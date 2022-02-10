; Estrutura representando o tabuleiro
(defstruct
    (board
        (:print-function
            (lambda (struct stream depth)
                (declare (ignore depth))
                (loop for row across (board-cells struct) do
                    (loop for cell across row do
                        (format stream "~a " cell)
                    )
                    (format stream "~%")
                )
            )
        )
    )
    size    ; Tamanho (nxn)
    cells   ; Células
)

; Estrutura representando uma célula do tabuleiro
(defstruct
    (cell
        (:print-function
            (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "~A-~A" (cell-group struct) (if (cell-value struct) (cell-value struct) "_"))
            )
        )
    )
    group   ; Bloco
    value   ; Valor
)

; Cria uma célula conforme o padrão GRUPO-VALOR
(defun cell-from-string(str)
    (setq group "")
    (setq value "")

    (setq current "")
    (loop for char across str do
        (cond
            ((eq char #\-)
                (setq group current)
                (setq current "")
            )
            (t
                (setq current (concatenate 'string current (string char)))
            )
        )
    )

    (if (string= current "_")
        (setq value nil)
        (setq value (parse-integer current))
    )


    (setq cell (make-cell :group group :value value))
)

; Separa uma string em uma lista de string, com base em um caractere
(defun row-from-string(base)
    (setq cells (list))
    (setq word "")

    (loop for char across base do
        (cond
            ((string= char #\Space)
                (setq cells (append cells (list (cell-from-string word))))
                (setq word "")
            )
            (t
                (setq word (concatenate 'string word (string char)))
            )
        )
    )

    ; último elemento
    (setq cells (append cells (list (cell-from-string word))))
)

; Construção de um tabuleiro a partir de um arquivo
(defun load-board(size filename)
    (with-open-file (stream filename)
        (make-board :size size :cells (make-array size :initial-contents (loop for line = (read-line stream nil) while line collect (make-array size :initial-contents (row-from-string line)))))
    )
)

(defun is-solved(board)
    (not (member nil (loop for row across (board-cells board) collect (loop for cell across row never (eq (cell-value cell) nil)))))
)

; Função responsável por resolver um tabuleiro, caso haja solução
;; (defun solve(board)
;;     (princ "Resolvendo o tabuleiro...") 
;; )

(setq my-board (load-board 6 "Examples/6x6-Solution.txt"))
(princ my-board)
(princ (is-solved my-board))