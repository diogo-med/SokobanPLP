
import Data.Array



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

calculateNewPos :: Char -> (Int, Int) -> (Int, Int)
calculateNewPos tecla (y, x) = 
    case tecla of
        'w' -> (y - 1, x)
        's' -> (y + 1, x)
        'a' -> (y, x - 1)
        'd' -> (y, x + 1)
        _   -> (y, x)


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


-- -- ...existing code...

-- Nova função que retorna (posição_jogador, mapa_atualizado)
moveWithMap :: Char -> (Int, Int) -> Array (Int, Int) Tile -> ((Int, Int), Array (Int, Int) Tile)
moveWithMap tecla (y, x) sokobanMap = 
    let newPos = calculateNewPos tecla (y, x)
    in if inBounds (bounds sokobanMap) newPos then 
        case sokobanMap ! newPos of
            Wall -> ((y, x), sokobanMap)  -- Não move, mapa inalterado
            Box -> 
                let boxNewPos = calculateNewPos tecla newPos
                in if inBounds (bounds sokobanMap) boxNewPos &&
                      sokobanMap ! boxNewPos /= Wall &&
                      sokobanMap ! boxNewPos /= Box
                   then 
                       -- ✅ ATUALIZA O MAPA: remove caixa da posição atual, coloca na nova
                       let updatedMap = sokobanMap // [(newPos, Floor), (boxNewPos, Box)]
                       in (newPos, updatedMap)
                   else ((y, x), sokobanMap)  -- Não pode empurrar, mapa inalterado
            _ -> (newPos, sokobanMap)  -- Move normalmente, mapa inalterado
       else ((y, x), sokobanMap)

-- Atualizar gameLoop para usar o mapa atualizado
gameLoopWithMap :: (Int, Int) -> Array (Int, Int) Tile -> IO ()
gameLoopWithMap currentPlayerPos currentMap = do
    putStrLn "=== SOKOBAN ==="
    printMap currentMap currentPlayerPos
    putStrLn $ "Posição: " ++ show currentPlayerPos
    putStrLn "Use w/a/s/d para mover, q para sair"
    
    tecla <- getChar
    putStrLn ""
    
    if tecla == 'q'
        then putStrLn "Fim do jogo!"
        else do
            let (newPos, newMap) = moveWithMap tecla currentPlayerPos currentMap
            if newPos == currentPlayerPos && tecla `elem` "wasd"
                then do
                    putStrLn "Movimento inválido!"
                    gameLoopWithMap currentPlayerPos currentMap
                else gameLoopWithMap newPos newMap

-- Nova função para iniciar com caixas
startGameWithBoxes :: IO ()
startGameWithBoxes = do
    -- Cria mapa inicial com algumas caixas
    let initialMap = sokobanMap // [((1,1), Box), ((3,3), Box), ((2,1), Wall)]
    gameLoopWithMap (2, 2) initialMap








-- move :: Char -> (Int, Int) -> Array (Int, Int) Tile -> Bool -> (Int, Int)
-- move tecla (y, x) sokobanMap isPlayer = 
--     let newPos = calculateNewPos tecla (y, x)
--     in if inBounds (bounds sokobanMap) newPos then 
--         let tileAtNewPos = sokobanMap ! newPos   
--         in case tileAtNewPos of
--             Wall -> (y, x)  -- Parede: não move
--             Box -> 
--                 if isPlayer then
--                     -- Só jogador pode empurrar caixas
--                     let boxNewPos = move tecla newPos sokobanMap False  -- Caixa (False)
--                     in if boxNewPos /= newPos  -- Se caixa conseguiu mover
--                        then newPos             -- Jogador move
--                        else (y, x)             -- Caixa não moveu, jogador não move
--                 else 
--                     (y, x)  -- ❌ CONDIÇÃO DE PARADA: Caixa não empurra caixa
--             _ -> newPos  -- Floor/Mark: move normalmente
--        else (y, x)  -- Fora dos limites

-- -- Função wrapper para o jogador
-- movePlayer :: Char -> (Int, Int) -> Array (Int, Int) Tile -> (Int, Int)
-- movePlayer tecla pos sokobanMap = move tecla pos sokobanMap True



-- -- Atualizar gameLoop para usar a nova função
-- gameLoop :: (Int, Int) -> IO ()
-- gameLoop currentPlayerPos = do
--     putStrLn "=== SOKOBAN ==="
--     printMap sokobanMap currentPlayerPos
--     putStrLn $ "Posição: " ++ show currentPlayerPos
--     putStrLn "Use w/a/s/d para mover, q para sair"
    
--     tecla <- getChar
--     putStrLn ""
    
--     if tecla == 'q'
--         then putStrLn "Fim do jogo!"
--         else do
--             let newPos = movePlayer tecla currentPlayerPos sokobanMap
--             if newPos == currentPlayerPos && tecla `elem` "wasd"
--                 then do
--                     putStrLn "Movimento inválido!"
--                     gameLoop currentPlayerPos
--                 else gameLoop newPos

-- startGame :: IO ()
-- startGame = gameLoop (2, 2)

-- Sintaxe: array // [(posição, novoValor), ...]
-- sokobanMap // [(newPos, Floor), (boxNewPos, Box)]


