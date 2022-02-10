; Estrutura representando o tabuleiro
(defstruct
    (board
        (:print-function
            (lambda (struct stream depth)
                (declare (ignore depth))
                (loop for row in (board-cells struct) do
                    (loop for cell in row do
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
(defun board-load(size filename)
    (with-open-file (stream filename)
        (make-board :size size :cells (loop for line = (read-line stream nil) while line collect (row-from-string line)))
    )
)

(defun board-solved(board)
    (not (member nil (loop for row in (board-cells board) collect (loop for cell in row never (eq (cell-value cell) nil)))))
)

(defun board-at(board position)
    (nth (nth 1 position) (nth (nth 0 position) (board-cells board)))
)

(defun board-nothing-at(board position)
    (eq nil (cell-value (board-at board position)))
)

(defun get-next-cell(board)
    (loop for i from 0 below (board-size board) nconcing (loop for j from 0 below (board-size board) if (board-nothing-at board (list i j)) collect (list i j)))
)

; Função responsável por resolver um tabuleiro, caso haja solução
;; (defun solve(board)
;;     (princ "Resolvendo o tabuleiro...") 
;; )

(setq my-board (board-load 6 "Examples/6x6.txt"))
(print my-board)
(print (board-solved my-board))
(print (get-next-cell my-board))