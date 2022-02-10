; Estrutura representando uma célula do tabuleiro
(defstruct cell
    group   ; Bloco
    value   ; Valor
)

; Estrutura representando o tabuleiro
(defstruct board
    n       ; Tamanho do tabuleiro (nxn)
    cells   ; Matrix de células
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
        (make-array size :initial-contents (loop for line = (read-line stream nil) while line collect (make-array size :initial-contents (row-from-string line))))
    )
)

; Função responsável pela impressão de um tabuleiro
(defun print-board(board)
    (declare (ignore board))
    (write-line "Não implementado")
)

; Função responsável por resolver um tabuleiro, caso haja solução
(defun solve(board)
    (princ (print-board board))
    (princ "Resolvendo o tabuleiro...")
)

(princ (load-board 6 "Examples/6x6.txt"))