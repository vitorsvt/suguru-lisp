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
                (format stream "~A-~A" (cell-block struct) (if (cell-value struct) (cell-value struct) "_"))
            )
        )
    )
    block   ; Bloco
    value   ; Valor
)

; Cria uma célula conforme o padrão GRUPO-VALOR
(defun cell-from-string(str)
    (setq block "")
    (setq value "")

    (setq current "")
    (loop for char across str do
        (cond
            ((eq char #\-)
                (setq block current)
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


    (setq cell (make-cell :block block :value value))
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

; checa se o tabuleiro foi resolvido
(defun board-solved(board)
    (not 
        (member nil 
            (loop for row in (board-cells board) collect 
                (loop for cell in row never 
                    (eq (cell-value cell) nil)
                )
            )
        )
    )
)

; Verifica se a posição é valida, isto é, está dentro dos limites do tabuleiro
(defun valid-position(n i j)
    (and (>= i 0) (>= j 0) (< i n) (< j n))
)

; retorna uma celula em uma posição
(defun board-at(board position)
    (let
        (
            (i (nth 0 position))
            (j (nth 1 position))
        )
        (if (valid-position (board-size board) i j) (nth j (nth i (board-cells board))) nil)
    )
)

; Adiciona um valor ao tabuleiro
(defun board-set(board position value)
    (let
        (
            (i (nth 0 position))
            (j (nth 1 position))
            (cell (board-at board position))
        )
        (setf (nth j (nth i (board-cells board))) (make-cell :block (cell-block cell) :value value))
    )
)

; Retorna o valor de uma celula com base na sua posicao
(defun value-at(board position)
    (cell-value (board-at board position))
)

; Retorna verdadeiro se não há nenhum valor na célula
(defun board-nothing-at(board position)
    (eq nil (cell-value (board-at board position)))
)

; Retorna todas as células do tabuleiro, em uma lista unidimensional
(defun board-cells-1d(board)
    (loop for row in (board-cells board) nconcing (loop for cell in row collect cell))
)

; Retorna os possíveis números para um bloco
(defun block-options(board block)
    (let*
        (
            (block-cells (board-block board block))
            (block-values (loop for cell in block-cells if (cell-value cell) collect (cell-value cell)))
        )
        (loop for i from 1 to (length block-cells) if (not (member i block-values)) collect i)
    )
)

; Posicao de todos os vizinhos imediatos a uma celula
(defun get-neighbor-positions(i j)
    (loop for a from -1 to 1 nconcing 
        (loop for b from -1 to 1
            if (not (and (= a 0) (= b 0))) collect (list (+ i a) (+ j b))
        )
    )
)

; Vizinhos de uma posicao (x,y)
(defun get-neighbors(board i j)
    (loop for neighbor in 
        (loop for position in 
            (get-neighbor-positions i j) collect (board-at board position)
        ) if neighbor collect neighbor)
)

; O valor dos vizinhos
(defun get-neighbor-values(board i j)
    (loop for neighbor in (get-neighbors board i j) if (cell-value neighbor) collect (cell-value neighbor))
)


; Retorna todas as células de um bloco
(defun board-block(board block)
    (loop for cell in (board-cells-1d board) if (string= (cell-block cell) block) collect cell)
)

; As opções validas para uma determinada celula ou Nothing.
(defun get-cell-options(board position)
    (let 
        (
            (cell (board-at board position))
            (i (nth 0 position))
            (j (nth 1 position))
        )
        (loop for option in (block-options board (cell-block cell)) 
            if (not (member option (get-neighbor-values board i j))) collect option
        )
    )
)

; Checa todas as células do tabuleiro retornando as que não foram respondidas
; Retorna a posição com menos opções, ou nil caso não haja mais posições livres
(defun get-next-cell(board)
    (reduce 
        (lambda (a b) 
            (get-next-cell* board a b)) 
            (loop for i from 0 below 
                (board-size board) nconcing 
                    (loop for j from 0 below (board-size board)
                        if (board-nothing-at board (list i j)) 
                            collect (list i j)
                    )
            )
    )
)

; Itera sobre as células não respondidas, retornando a posição com menos possibilidades
(defun get-next-cell*(board position-a position-b)
    (cond
        ((null position-a)
            nil
        )
        ((null position-b)
            nil
        )
        (t
            (let
                (
                    (a (get-cell-options board position-a))
                    (b (get-cell-options board position-b))
                )
                (if (< (length a) (length b)) position-a position-b)
            )
        )
    )
)

; Função responsável por resolver um tabuleiro, caso haja solução
(defun solve(board)
    (if
        (board-solved board)
        board
        (let*
            (
                (pos (get-next-cell board))
                (options (get-cell-options board pos))
            )
            (solve* board pos options)
        )
    )
)

; Função auxiliar de solução que realiza o backtracking.
(defun solve*(board pos options)
    (cond
        ((null options)
            nil
        )
        (t
            (let
                ((value (pop options)))
                (board-set board pos value)
                (case (solve board)
                    ((nil)
                        (board-set board pos nil)
                        (solve* board pos options)
                    )
                    (otherwise
                        t
                    )
                )
            )
        )
    )
)

(defun main()

(setq my-board (board-load 10 "Examples/10x10.txt"))

(print my-board)
(solve my-board)
(print my-board)
)

(main)