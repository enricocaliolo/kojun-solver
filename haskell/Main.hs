-- Importa funções específicas de bibliotecas necessárias
import Data.List (elemIndex)  -- Importa elemIndex, que encontra o índice de um elemento em uma lista
import Data.Maybe (isNothing) -- Importa isNothing, que verifica se um valor Maybe é Nothing
import Games (board_6x6, regions_6x6, board_10x10, regions_10x10, board_17x17, regions_17x17) 
-- Importa tabelas e regiões de Games
import Types (Board, Regions, Cell, Region)
-- Importa tipos de Types

-- Função que imprime o tabuleiro no console
printBoard :: Board -> IO ()
printBoard board = do
    mapM_ print board         -- Itera sobre cada linha do tabuleiro e imprime-a
    putStrLn "------------------"  -- Imprime uma linha de separação

-- Função que encontra a primeira célula vazia no tabuleiro
findEmptyCell :: Board -> Maybe Cell
findEmptyCell board = findEmptyCell' board 0
  where
    findEmptyCell' [] _ = Nothing  -- Se a lista de linhas está vazia, retorna Nothing
    findEmptyCell' (row:rows) i = case elemIndex 0 row of
        Nothing -> findEmptyCell' rows (i + 1) -- Se não encontra 0 na linha, continua para a próxima
        Just j -> Just (i, j)  -- Se encontra 0, retorna a posição como Maybe Cell

-- Função que encontra a região que contém uma célula específica
findRegion :: Cell -> Regions -> (Int, Region)
findRegion cell regions = 
    case [ (r, cells) | (r, cells) <- regions, cell `elem` cells ] of
        [] -> error $ "No region found for cell: " ++ show cell  -- Se não encontra a célula, lança um erro
        (r, cells):_ -> (r, cells)  -- Retorna a região encontrada

-- Função que verifica se um número é válido para uma célula específica no tabuleiro
isValid :: Board -> Regions -> Cell -> Int -> Bool
isValid board regions cell num =
    let (region, regionCells) = findRegion cell regions in
    notElem num (map (boardValue board) regionCells) &&  -- Verifica se o número não está na região
    num <= length regionCells &&  -- Verifica se o número não excede o tamanho da região
    checkAdjacencies board regions cell num region  -- Verifica as adjacências

-- Função que obtém o valor de uma célula no tabuleiro
boardValue :: Board -> Cell -> Int
boardValue board (i, j) = (board !! i) !! j

-- Função que verifica as adjacências de uma célula para validar o número
checkAdjacencies :: Board -> Regions -> Cell -> Int -> Int -> Bool
checkAdjacencies board regions (x, y) num region =
    checkAbove board regions (x, y) num region &&  -- Verifica a célula acima
    checkBelow board regions (x, y) num region &&  -- Verifica a célula abaixo
    checkLeft board (x, y) num &&  -- Verifica a célula à esquerda
    checkRight board (x, y) num  -- Verifica a célula à direita

-- Função que verifica a célula acima
checkAbove :: Board -> Regions -> Cell -> Int -> Int -> Bool
checkAbove board regions (x, y) num region =
    let newX = x - 1 in
    if newX >= 0 then
        let aboveValue = boardValue board (newX, y) in
        if aboveValue == 0 then True
        else
            let (aboveRegion, _) = findRegion (newX, y) regions in
            (aboveRegion /= region || aboveValue > num) && aboveValue /= num
    else True

-- Função que verifica a célula abaixo
checkBelow :: Board -> Regions -> Cell -> Int -> Int -> Bool
checkBelow board regions (x, y) num region =
    let newX = x + 1 in
    if newX < length board then
        let belowValue = boardValue board (newX, y) in
        if belowValue == 0 then True
        else
            let (belowRegion, _) = findRegion (newX, y) regions in
            (belowRegion /= region || belowValue < num) && belowValue /= num
    else True

-- Função que verifica a célula à esquerda
checkLeft :: Board -> Cell -> Int -> Bool
checkLeft board (x, y) num =
    let newY = y - 1 in
    if newY >= 0 then
        let leftValue = boardValue board (x, newY) in
        leftValue /= num
    else True

-- Função que verifica a célula à direita
checkRight :: Board -> Cell -> Int -> Bool
checkRight board (x, y) num =
    let newY = y + 1 in
    if newY < length (head board) then
        let rightValue = boardValue board (x, newY) in
        rightValue /= num
    else True

-- Função que resolve o tabuleiro de jogo
solve :: Board -> Regions -> Maybe Board
solve board regions = case findEmptyCell board of
    Nothing -> Just board  -- Se não há células vazias, retorna o tabuleiro resolvido
    Just emptyCell -> solveCell board regions emptyCell [1..length board]  -- Tenta resolver a célula vazia

-- Função auxiliar para resolver uma célula específica
solveCell :: Board -> Regions -> Cell -> [Int] -> Maybe Board
solveCell board regions emptyCell [] = Nothing  -- Se não há mais números para tentar, retorna Nothing
solveCell board regions emptyCell (num:nums) =
    if isValid board regions emptyCell num
    then let newBoard = updateBoard board emptyCell num in
        case solve newBoard regions of
            Just solvedBoard -> Just solvedBoard  -- Se resolver o tabuleiro com sucesso, retorna-o
            Nothing -> solveCell board regions emptyCell nums  -- Continua tentando com os outros números
    else solveCell board regions emptyCell nums  -- Continua tentando com os outros números

-- Função que atualiza o tabuleiro com um novo valor em uma célula específica
updateBoard :: Board -> Cell -> Int -> Board
updateBoard board (x, y) num = 
    take x board ++ [take y (board !! x) ++ [num] ++ drop (y + 1) (board !! x)] ++ drop (x + 1) board

-- Função principal que inicializa e resolve o tabuleiro de jogo
main :: IO ()
main = do
    let board = board_10x10  -- Define o tabuleiro inicial
    let regions = regions_10x10  -- Define as regiões do tabuleiro
    let solution = solve board regions  -- Tenta resolver o tabuleiro
    case solution of
        Just solvedBoard -> printBoard solvedBoard  -- Se resolver com sucesso, imprime o tabuleiro resolvido
        Nothing -> putStrLn "No solution found."  -- Se não encontrar solução, imprime uma mensagem de erro
