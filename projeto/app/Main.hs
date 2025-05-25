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

moveLine :: [Int] -> [Int]
moveLine xs = let noZeros = filter (/= 0) xs
                  merged = merge noZeros
              in merged ++ replicate (boardSize - length merged) 0
  where
    merge (x:y:zs)
      | x == y = x*2 : merge zs
      | otherwise = x : merge (y:zs)
    merge xs = xs

moveLeft, moveRight, moveUp, moveDown :: Board -> Board
moveLeft = map moveLine
moveRight = map (reverse . moveLine . reverse)
moveUp = rotateBoard . rotateBoard . rotateBoard . moveLeft . rotateBoard
moveDown = rotateBoard . moveLeft . rotateBoard . rotateBoard . rotateBoard

gameOver :: Board -> Bool
gameOver b = all (\f -> f b == b) [moveLeft, moveRight, moveUp, moveDown]

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
  boardView <- renderBoard board0
  body <- getBody window
  container <- UI.div # set UI.style
    [ ("display", "flex")
    , ("justify-content", "center")
    , ("align-items", "center")
    , ("height", "100vh") -- para centralizar verticalmente
    ]

  void $ element container #+ [element boardView]
  void $ element body #+ [element container]

  on UI.keydown body $ \keyCode -> do
    board <- liftIO $ readIORef boardVar
    let moveFn = case keyCode of
          37 -> moveLeft
          38 -> moveUp
          39 -> moveRight
          40 -> moveDown
          _  -> id
    let newBoard = moveFn board
    if newBoard == board then return () else do
      b2 <- liftIO $ addRandomTile newBoard
      liftIO $ writeIORef boardVar b2
      void $ element boardView # set children []
      newView <- renderBoard b2
      void $ element boardView #+ [element newView]

      when (any (elem 2048) b2) $ showAlert window "Você venceu!"
      when (gameOver b2) $ showAlert window "Game Over!"

main :: IO ()
main = do
  startBoard <- addRandomTile =<< addRandomTile emptyBoard
  startGUI defaultConfig (setup startBoard)
