
import Data.Array
import Text.XHtml (theclass)

data Tile = Wall | Floor | Box | Player | Mark deriving (Show, Eq)

sokobanMap :: Array (Int, Int) Tile
sokobanMap = array ((0, 0), (4, 4)) 
             [((i, j), Floor) | i <- [0..4], j <- [0..4]]

tileToChar :: Tile -> Char
tileToChar Wall = '#'
tileToChar Floor = '.'
tileToChar Box = 'B'
tileToChar Player = '@'
tileToChar Mark = 'X'

-- Helper function to check if position is within bounds
inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((minY, minX), (maxY, maxX)) (y, x) =
    x >= minX && x <= maxX && y >= minY && y <= maxY



move :: Char -> (Int, Int) -> Array (Int, Int) Tile -> (Int, Int)
move tecla (y, x) sokobanMap = 
    let newPos = case tecla of
                    'w' -> (y - 1, x)
                    's' -> (y + 1, x)
                    'a' -> (y, x - 1)
                    'd' -> (y, x + 1)
                    _   -> (y, x)
    in if inBounds (bounds sokobanMap) newPos 
       then newPos          -- Retorna nova posição
       else (y, x)          -- Mantém posição atual

printMap :: Array (Int, Int) Tile -> (Int, Int) -> IO ()
printMap gameMap playerPos = do
    let ((minY, minX), (maxY, maxX)) = bounds gameMap
    mapM_ (\y -> do
        mapM_ (\x -> do
            let tile = if (y, x) == playerPos then Player else gameMap ! (y, x)
            putChar (tileToChar tile)
            ) [minX..maxX]
        putStrLn ""
        ) [minY..maxY]
    putStrLn ""

-- Atualizar o gameLoop para mostrar o mapa
gameLoop :: (Int, Int) ->  IO ()
gameLoop currentPlayerPos = do
    putStrLn "=== SOKOBAN ==="
    printMap sokobanMap currentPlayerPos
    putStrLn $ "Posição: " ++ show currentPlayerPos
    putStrLn "Use w/a/s/d para mover, q para sair"
    
    tecla <- getChar
    putStrLn ""  -- Nova linha após input
    
    if tecla == 'q'
        then putStrLn "Fim do jogo!"
        else do
            let newPos = move tecla currentPlayerPos sokobanMap
            if newPos == currentPlayerPos && tecla `elem` "wasd"
                then do
                    putStrLn "Movimento inválido!"
                    gameLoop currentPlayerPos
                else gameLoop newPos

-- Função para iniciar o jogo
startGame :: IO ()
startGame = gameLoop (2, 2)  -- Posição inicial

