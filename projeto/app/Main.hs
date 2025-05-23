{-# LANGUAGE LambdaCase #-}

import System.Random (randomRIO)
import System.IO (hFlush, stdout)

type Board = [[Int]]

boardSize :: Int
boardSize = 4

emptyBoard :: Board
emptyBoard = replicate boardSize (replicate boardSize 0)

showBoard :: Board -> String
showBoard = unlines . map (unwords . map showCell)
  where
    showCell 0 = "."
    showCell n = show n

addRandomTile :: Board -> IO Board
addRandomTile board = do
  let emptyCells = [(r,c) | r <- [0..boardSize-1], c <- [0..boardSize-1], (board !! r) !! c == 0]
  if null emptyCells
    then return board
    else do
      (r,c) <- (emptyCells !!) <$> (randomRIO (0, length emptyCells -1) :: IO Int)
      x <- (randomRIO (1,10) :: IO Int)
      let val = if x == 10 then 4 else 2
      let newRow = take c (board !! r) ++ [val] ++ drop (c+1) (board !! r)
      return $ take r board ++ [newRow] ++ drop (r+1) board


rotateBoard :: Board -> Board
rotateBoard = reverse . transpose
  where transpose ([]:_) = []
        transpose x = map head x : transpose (map tail x)

moveLine :: [Int] -> [Int]
moveLine xs = let noZeros = filter (/= 0) xs
                  merged = merge noZeros
              in merged ++ replicate (boardSize - length merged) 0
  where
    merge (x:y:zs)
      | x == y = x*2 : merge zs
      | otherwise = x : merge (y:zs)
    merge xs = xs

moveLeft :: Board -> Board
moveLeft = map moveLine

moveRight :: Board -> Board
moveRight = map (reverse . moveLine . reverse)

moveUp :: Board -> Board
moveUp = rotateBoard . rotateBoard . rotateBoard . moveLeft . rotateBoard

moveDown :: Board -> Board
moveDown = rotateBoard . moveLeft . rotateBoard . rotateBoard . rotateBoard

gameOver :: Board -> Bool
gameOver b = all (\f -> f b == b) [moveLeft, moveRight, moveUp, moveDown]

gameLoop :: Board -> IO ()
gameLoop board = do
  putStrLn $ showBoard board
  if any (elem 2048) board
    then putStrLn "Você venceu!"
    else if gameOver board
      then putStrLn "Game Over!"
      else do
        putStr "Movimento (w/a/s/d): "
        hFlush stdout
        cmd <- getLine
        let newBoard = case cmd of
              "a" -> moveLeft board
              "d" -> moveRight board
              "w" -> moveUp board
              "s" -> moveDown board
              _   -> board
        if newBoard == board
          then do
            putStrLn "Movimento inválido!"
            gameLoop board
          else do
            b2 <- addRandomTile newBoard
            gameLoop b2

main :: IO ()
main = do
  b0 <- addRandomTile =<< addRandomTile emptyBoard
  putStrLn "Jogo 2048 em Haskell! Use w/a/s/d para mover."
  gameLoop b0
