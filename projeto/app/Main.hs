{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (transpose)
import System.Random (randomRIO)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

type Board = [[Int]]
boardSize :: Int
boardSize = 4

emptyBoard :: Board
emptyBoard = replicate boardSize (replicate boardSize 0)

showCell :: Int -> String
showCell 0 = "."
showCell n = show n

addRandomTile :: Board -> IO Board
addRandomTile board = do
  let emptyCells = [(r,c) | r <- [0..boardSize-1], c <- [0..boardSize-1], (board !! r) !! c == 0]
  if null emptyCells
    then return board
    else do
      (r,c) <- (emptyCells !!) <$> (randomRIO (0, length emptyCells - 1) :: IO Int)
      x <- randomRIO (1, 10) :: IO Int
      let val = if x == 10 then 4 else 2
      let newRow = take c (board !! r) ++ [val] ++ drop (c + 1) (board !! r)
      return $ take r board ++ [newRow] ++ drop (r + 1) board

rotateBoard :: Board -> Board
rotateBoard = reverse . transpose

-- move e calcula a pontuacao de uma linha
moveLineWithScore :: [Int] -> ([Int], Int)
moveLineWithScore xs = 
  let noZeros = filter (/= 0) xs
      (merged, score) = merge noZeros
      padded = merged ++ replicate (boardSize - length merged) 0
  in (padded, score)
  where
    merge (x:y:zs)
      | x == y =
          let (rest, s) = merge zs
          in ((x * 2) : rest, x * 2 + s)
      | otherwise =
          let (rest, s) = merge (y:zs)
          in (x : rest, s)
    merge xs = (xs, 0)

-- aplicando moveLineWithScore em todas as linhas
moveBoardLeftWithScore :: Board -> (Board, Int)
moveBoardLeftWithScore b = unzipWithSum (map moveLineWithScore b)

unzipWithSum :: [([Int], Int)] -> (Board, Int)
unzipWithSum xs = (map fst xs, sum (map snd xs))

-- generalizacao para todas as direcoes
moveWithScore :: Int -> Board -> (Board, Int)
moveWithScore dir board =
  case dir of
    37 -> moveBoardLeftWithScore board
    39 -> let (b, s) = moveBoardLeftWithScore (map reverse board)
          in (map reverse b, s)
    38 -> let (b, s) = moveBoardLeftWithScore (rotateBoard board)
          in (rotateBoard . rotateBoard . rotateBoard $ b, s)
    40 -> let (b, s) = moveBoardLeftWithScore (rotateBoard . rotateBoard . rotateBoard $ board)
          in (rotateBoard b, s)
    _  -> (board, 0)

gameOver :: Board -> Bool
gameOver b = all (\f -> f b == b) [moveLeft, moveRight, moveUp, moveDown]
  where
    moveLeft = map (fst . moveLineWithScore)
    moveRight = map (reverse . fst . moveLineWithScore . reverse)
    moveUp = rotateBoard . rotateBoard . rotateBoard . moveLeft . rotateBoard
    moveDown = rotateBoard . moveLeft . rotateBoard . rotateBoard . rotateBoard

-- Renderiza o tabuleiro
renderBoard :: Board -> UI Element
renderBoard board = UI.div # set UI.style [("display", "inline-block"), ("font-family", "monospace")] #+
  [ UI.div # set UI.style [("display", "flex")] #+
      [ UI.div # set UI.style (cellStyle (board !! r !! c)) #+ [string (showCell (board !! r !! c))] | c <- [0..boardSize-1]]
  | r <- [0..boardSize-1]]
  where
    cellStyle 0 = [("width","50px"), ("height","50px"), ("margin","2px"), ("border","1px solid #ccc"),
                   ("display","flex"), ("justify-content","center"), ("align-items","center"), ("background","#eee")]
    cellStyle _ = [("width","50px"), ("height","50px"), ("margin","2px"), ("border","1px solid #ccc"),
                   ("display","flex"), ("justify-content","center"), ("align-items","center"),
                   ("background","#ffa"), ("font-weight","bold"), ("font-size","18px")]

showAlert :: Window -> String -> UI ()
showAlert window msg = do
  alertBox <- UI.div # set UI.text msg
                     # set UI.style [("background-color", "lightyellow"),
                                     ("border", "1px solid black"),
                                     ("padding", "10px"),
                                     ("position", "fixed"),
                                     ("top", "40%"),
                                     ("left", "40%"),
                                     ("z-index", "1000")]

  closeBtn <- UI.button # set UI.text "Fechar"
  on UI.click closeBtn $ \_ -> UI.delete alertBox
  element alertBox #+ [element closeBtn]

  body <- getBody window
  void $ element body #+ [element alertBox]

setup :: Board -> Window -> UI ()
setup board0 window = do
  return window # set UI.title "2048"

  boardVar <- liftIO $ newIORef board0
  scoreVar <- liftIO $ newIORef 0
  scoreDisplay <- UI.span # set UI.text "Score: 0"

  boardView <- renderBoard board0
  body <- getBody window
  container <- UI.div # set UI.style
    [ ("display", "flex")
    , ("flex-direction", "column")
    , ("justify-content", "center")
    , ("align-items", "center")
    , ("height", "100vh")
    ]

  void $ element container #+ [element scoreDisplay, element boardView]
  void $ element body #+ [element container]

  on UI.keydown body $ \keyCode -> do
    board <- liftIO $ readIORef boardVar
    let (newBoard, points) = moveWithScore keyCode board

    if newBoard == board then return () else do
      liftIO $ modifyIORef scoreVar (+ points)

      b2 <- liftIO $ addRandomTile newBoard
      liftIO $ writeIORef boardVar b2

      void $ element boardView # set children []
      newView <- renderBoard b2
      void $ element boardView #+ [element newView]

      when (any (elem 2048) b2) $ showAlert window "Você venceu!"
      when (gameOver b2) $ showAlert window "Game Over!"

      newScore <- liftIO $ readIORef scoreVar
      void $ element scoreDisplay # set UI.text ("Score: " ++ show newScore)

main :: IO ()
main = do
  startBoard <- addRandomTile =<< addRandomTile emptyBoard
  startGUI defaultConfig (setup startBoard)
