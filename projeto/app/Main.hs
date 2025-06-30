-- File: app/Main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (transpose, isPrefixOf)
import System.Random (randomRIO)
import Prelude hiding (lookup)
import qualified Data.Map as Map
import GHC.Generics (Generic)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import CRUD.Query (User(..))
import Data.Aeson

data Leaderboard = Leaderboard { players :: [User] }
  deriving (Generic, FromJSON)

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

moveBoardLeftWithScore :: Board -> (Board, Int)
moveBoardLeftWithScore b = unzipWithSum (map moveLineWithScore b)

unzipWithSum :: [([Int], Int)] -> (Board, Int)
unzipWithSum xs = (map fst xs, sum (map snd xs))

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

showPodiumBox :: Window -> [User] -> UI ()
showPodiumBox window users = do
  podiumBox <- UI.div # set UI.style [("background-color", "rgba(255, 255, 170, 0.9)"),
                                     ("border", "1px solid #888"),
                                     ("border-radius", "8px"),
                                     ("padding", "20px"),
                                     ("width", "300px"),
                                     ("text-align", "center"),
                                     ("position", "fixed"),
                                     ("top", "50%"),
                                     ("left", "50%"),
                                     ("transform", "translate(-50%, -50%)"),
                                     ("z-index", "1000"),
                                     ("box-shadow", "0 4px 8px rgba(0,0,0,0.2)")]
  userListItems <- mapM (renderUser) (zip [1..] users)
  userList <- UI.mkElement "ol" #+ (map UI.element userListItems)
  closeBtn <- UI.button # set UI.text "Fechar"
  on UI.click closeBtn $ \_ -> UI.delete podiumBox
  element podiumBox #+
    [ UI.h3 #+ [string "Pódio de Melhores Jogadores"]
    , element userList
    , element closeBtn
    ]
  body <- getBody window
  void $ element body #+ [element podiumBox]
  where
    renderUser (rank, User _ name _ score) =
      UI.li # set UI.style [("text-align", "left"), ("margin-bottom", "5px")] #+
        [string (show rank ++ ". " ++ name ++ " - Recorde: " ++ show score)]

showLoginBox :: Window -> String -> UI ()
showLoginBox window msg = do
  -- Elementos da UI
  loginBox    <- UI.div # set UI.text msg
  userEntry   <- UI.entry (pure "")
  -- ALTERADO: Criação do campo de senha separada da modificação de atributo
  passEntry   <- UI.entry (pure "")
  void $ element passEntry # set (attr "type") "password"

  loginBtn    <- UI.button #+ [string "Entrar"]
  sairBtn     <- UI.button #+ [string "Sair"]
  closeBtn    <- UI.button #+ [string "Fechar"]

  -- Composição do Layout
  element loginBox #+
      [ UI.column
          [ UI.row [string "Login"]
          , UI.row [string "Usuário: ", element userEntry]
          , UI.row [string "Senha: ", element passEntry]
          , UI.row [element loginBtn, element sairBtn, element closeBtn]
          ]
      ]

  -- Estilo
  void $ element loginBox # set UI.style
    [("background-color", "rgba(255, 255, 170, 0.9)"), ("border-radius", "8px"), ("padding", "20px"),
     ("position", "fixed"), ("top", "50%"), ("left", "50%"), ("transform", "translate(-50%, -50%)"),
     ("z-index", "1000"), ("box-shadow", "0 4px 8px rgba(0,0,0,0.2)")]

  -- Comportamento dos botões
  on UI.click loginBtn $ \_ -> do
    username <- get UI.value (getElement userEntry)
    password <- get UI.value (getElement passEntry)
    let jsCode = mconcat
          [ "fetch('http://localhost:8080/login', {",
            "method: 'POST', headers: { 'Content-Type': 'application/json' },",
            "body: JSON.stringify({ loginName: '", username, "', loginPassword: '", password, "' })",
            "}).then(response => response.json()).then(success => {",
            "if (success) {",
            "  document.getElementById('userDisplay').innerText = 'Olá, ", username, "!';",
            "  localStorage.setItem('user', JSON.stringify({ username: '", username, "' }));",
            "} else { alert('Usuário não existe ou senha incorreta!'); }",
            "});" ]
    runFunction $ ffi jsCode
    UI.delete loginBox -- Fecha a janela após a tentativa

  on UI.click sairBtn $ \_ -> do
    let jsCode = "document.getElementById('userDisplay').innerText = ''; localStorage.removeItem('user');"
    runFunction $ ffi jsCode
    UI.delete loginBox -- Fecha a janela

  on UI.click closeBtn $ \_ -> UI.delete loginBox

  -- Adiciona o elemento final ao corpo da página
  body <- getBody window
  void $ element body #+ [element loginBox]

showCadastroBox :: Window -> String -> UI ()
showCadastroBox window msg = do
  -- Elementos da UI
  cadastroBox   <- UI.div # set UI.text msg
  userEntry     <- UI.entry (pure "")
  -- ALTERADO: Criação do campo de senha separada da modificação de atributo
  passEntry     <- UI.entry (pure "")
  void $ element passEntry # set (attr "type") "password"

  cadastroBtn   <- UI.button #+ [string "Cadastrar"]
  closeBtn      <- UI.button #+ [string "Fechar"]

  -- Composição do Layout
  element cadastroBox #+
      [ UI.column
          [ UI.row [string "Cadastro"]
          , UI.row [string "Usuário: ", element userEntry]
          , UI.row [string "Senha: ", element passEntry]
          , UI.row [element cadastroBtn, element closeBtn]
          ]
      ]

  -- Estilo
  void $ element cadastroBox # set UI.style
    [("background-color", "rgba(255, 255, 170, 0.9)"), ("border-radius", "8px"), ("padding", "20px"),
     ("position", "fixed"), ("top", "50%"), ("left", "50%"), ("transform", "translate(-50%, -50%)"),
     ("z-index", "1000"), ("box-shadow", "0 4px 8px rgba(0,0,0,0.2)")]

  -- Comportamento dos botões
  on UI.click cadastroBtn $ \_ -> do
    username <- get UI.value (getElement userEntry)
    password <- get UI.value (getElement passEntry)
    let score = "0"
    let jsCode = mconcat
          [ "fetch('http://localhost:8080/users', {",
            "method: 'POST', headers: { 'Content-Type': 'application/json' },",
            "body: JSON.stringify({ userName: '", username, "', userPassword: '", password, "', userScore: ", score, "})",
            "}).then(() => { alert('Cadastro sucedido!'); }).catch(() => { alert('Cadastro não sucedido!'); });" ]
    runFunction $ ffi jsCode
    UI.delete cadastroBox -- Fecha a janela após a tentativa

  on UI.click closeBtn $ \_ -> UI.delete cadastroBox

  -- Adiciona o elemento final ao corpo da página
  body <- getBody window
  void $ element body #+ [element cadastroBox]

setup :: Board -> Window -> UI ()
setup board0 window = do
  return window # set UI.title "2048"

  boardVar <- liftIO $ newIORef board0
  scoreVar <- liftIO $ newIORef 0
  userDisplay <- UI.span # set UI.text "" # set UI.id_ "userDisplay"
  loginDisplay <- UI.button #. "button" #+ [string "Login"]
  cadastroDisplay <- UI.button #. "button" #+ [string "Cadastro"]
  podiumDisplay <- UI.button #. "button" #+ [string "Pódio"]
  scoreDisplay <- UI.span # set UI.text "Score: 0"
  boardView <- renderBoard board0

  -- Layout principal
  body <- getBody window
  buttons <- UI.div # set UI.style [("display", "flex"), ("flex-direction", "row"), ("justify-content", "center"), ("gap", "50px"), ("padding", "2vh"), ("cursor", "pointer")]
  container <- UI.div # set UI.style [("display", "flex"), ("flex-direction", "column"), ("justify-content", "center"), ("align-items", "center"), ("height", "100vh"), ("overflow", "hidden")]
  void $ element buttons #+ [element loginDisplay, element cadastroDisplay, element podiumDisplay]
  void $ element container #+ [element userDisplay, element buttons, element scoreDisplay, element boardView]
  void $ element body #+ [element container]

  -- Evento de movimento do jogo
  on UI.keydown body $ \keyCode -> do
    board <- liftIO $ readIORef boardVar
    let (newBoard, points) = moveWithScore keyCode board
    when (newBoard /= board) $ do
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

  -- Eventos de clique dos botões
  on UI.click loginDisplay $ \_ -> showLoginBox window ""
  on UI.click cadastroDisplay $ \_ -> showCadastroBox window ""
  on UI.click podiumDisplay $ \_ -> do
    let jsGetLeaderboard = "fetch('http://localhost:8080/leaderboard').then(res => res.json())"
    jsonValue <- callFunction $ ffi jsGetLeaderboard
    case fromJSON jsonValue :: Result Leaderboard of
      Success leaderboardData -> showPodiumBox window (players leaderboardData)
      Error err               -> showAlert window ("Erro ao carregar o pódio: " ++ err)

main :: IO ()
main = do
  startBoard <- addRandomTile =<< addRandomTile emptyBoard
  startGUI defaultConfig (setup startBoard)