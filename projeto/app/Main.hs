import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (transpose, isPrefixOf)
import System.Random (randomRIO)
import Prelude hiding (lookup)
import qualified Data.Map as Map

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import CRUD.Core

type Tabuleiro = [[Int]]
tamanhoTabuleiro :: Int
tamanhoTabuleiro = 4

tabuleiroVazio :: Tabuleiro
tabuleiroVazio = replicate tamanhoTabuleiro (replicate tamanhoTabuleiro 0)

mostrarCelula :: Int -> String
mostrarCelula 0 = "."
mostrarCelula n = show n

adicionarPecaAleatoria :: Tabuleiro -> IO Tabuleiro
adicionarPecaAleatoria tabuleiro = do
  let celulasVazias = [(l,c) | l <- [0..tamanhoTabuleiro-1], c <- [0..tamanhoTabuleiro-1], (tabuleiro !! l) !! c == 0]
  if null celulasVazias
    then return tabuleiro
    else do
      (l,c) <- (celulasVazias !!) <$> (randomRIO (0, length celulasVazias - 1) :: IO Int)
      x <- randomRIO (1, 10) :: IO Int
      let val = if x == 10 then 4 else 2
      let novaLinha = take c (tabuleiro !! l) ++ [val] ++ drop (c + 1) (tabuleiro !! l)
      return $ take l tabuleiro ++ [novaLinha] ++ drop (l + 1) tabuleiro

rotacionarTabuleiro :: Tabuleiro -> Tabuleiro
rotacionarTabuleiro = reverse . transpose

moverLinhaComPontuacao :: [Int] -> ([Int], Int)
moverLinhaComPontuacao xs = 
  let semZeros = filter (/= 0) xs
      (fundidos, pontos) = fundir semZeros
      preenchida = fundidos ++ replicate (tamanhoTabuleiro - length fundidos) 0
  in (preenchida, pontos)
  where
    fundir (x:y:zs)
      | x == y =
          let (resto, p) = fundir zs
          in ((x * 2) : resto, x * 2 + p)
      | otherwise =
          let (resto, p) = fundir (y:zs)
          in (x : resto, p)
    fundir xs = (xs, 0)

moverTabuleiroEsquerdaComPontuacao :: Tabuleiro -> (Tabuleiro, Int)
moverTabuleiroEsquerdaComPontuacao t = descompactarComSoma (map moverLinhaComPontuacao t)

descompactarComSoma :: [([Int], Int)] -> (Tabuleiro, Int)
descompactarComSoma xs = (map fst xs, sum (map snd xs))

moverComPontuacao :: Int -> Tabuleiro -> (Tabuleiro, Int)
moverComPontuacao direcao tabuleiro =
  case direcao of
    37 -> moverTabuleiroEsquerdaComPontuacao tabuleiro
    39 -> let (t, p) = moverTabuleiroEsquerdaComPontuacao (map reverse tabuleiro)
          in (map reverse t, p)
    38 -> let (t, p) = moverTabuleiroEsquerdaComPontuacao (rotacionarTabuleiro tabuleiro)
          in (rotacionarTabuleiro . rotacionarTabuleiro . rotacionarTabuleiro $ t, p)
    40 -> let (t, p) = moverTabuleiroEsquerdaComPontuacao (rotacionarTabuleiro . rotacionarTabuleiro . rotacionarTabuleiro $ tabuleiro)
          in (rotacionarTabuleiro t, p)
    _  -> (tabuleiro, 0)

fimDeJogo :: Tabuleiro -> Bool
fimDeJogo t = all (\f -> f t == t) [moverEsquerda, moverDireita, moverCima, moverBaixo]
  where
    moverEsquerda = map (fst . moverLinhaComPontuacao)
    moverDireita = map (reverse . fst . moverLinhaComPontuacao . reverse)
    moverCima = rotacionarTabuleiro . rotacionarTabuleiro . rotacionarTabuleiro . moverEsquerda . rotacionarTabuleiro
    moverBaixo = rotacionarTabuleiro . moverEsquerda . rotacionarTabuleiro . rotacionarTabuleiro . rotacionarTabuleiro

renderizarTabuleiro :: Tabuleiro -> UI Element
renderizarTabuleiro tabuleiro = UI.div # set UI.style [("display", "inline-block"), ("font-family", "monospace")] #+
  [ UI.div # set UI.style [("display", "flex")] #+
      [ UI.div # set UI.style (estiloCelula (tabuleiro !! l !! c)) #+ [string (mostrarCelula (tabuleiro !! l !! c))] | c <- [0..tamanhoTabuleiro-1]]
  | l <- [0..tamanhoTabuleiro-1]]
  where
    estiloCelula 0 = [("width","50px"), ("height","50px"), ("margin","2px"), ("border","1px solid #ccc"),
                      ("display","flex"), ("justify-content","center"), ("align-items","center"), ("background","#eee")]
    estiloCelula _ = [("width","50px"), ("height","50px"), ("margin","2px"), ("border","1px solid #ccc"),
                      ("display","flex"), ("justify-content","center"), ("align-items","center"),
                      ("background","#ffa"), ("font-weight","bold"), ("font-size","18px")]

configurarInterface :: Tabuleiro -> Window -> UI ()
configurarInterface tabuleiroInicial janela = do
  return janela # set UI.title "2048"

  estadoTabuleiro <- liftIO $ newIORef tabuleiroInicial
  estadoPontuacao <- liftIO $ newIORef 0

  textoUsuario <- UI.span # set UI.text "" # set UI.id_ "userDisplay"
  botaoLogin <- UI.button #. "button" #+ [string "Login"]
  botaoCadastro <- UI.button #. "button" #+ [string "Cadastro"]
  textoPontuacao <- UI.span # set UI.text "Score: 0"

  visualTabuleiro <- renderizarTabuleiro tabuleiroInicial

  corpo <- getBody janela
  botoes <- UI.div # set UI.style
    [ ("display", "flex")
    , ("flex-direction", "row")
    , ("justify-content", "center")
    , ("gap", "50px")
    , ("padding", "2vh")
    , ("cursor", "pointer")
    ]

  container <- UI.div # set UI.style
    [ ("display", "flex")
    , ("flex-direction", "column")
    , ("justify-content", "center")
    , ("align-items", "center")
    , ("height", "100vh")
    , ("overflow ", "hidden")
    ]

  void $ element botoes #+ [element botaoLogin, element botaoCadastro]
  void $ element container #+ [element textoUsuario, element botoes, element textoPontuacao, element visualTabuleiro]
  void $ element corpo #+ [element container]

  on UI.keydown corpo $ \tecla -> do
    tabuleiro <- liftIO $ readIORef estadoTabuleiro
    let (novoTabuleiro, pontos) = moverComPontuacao tecla tabuleiro

    if novoTabuleiro == tabuleiro then return () else do
      liftIO $ modifyIORef estadoPontuacao (+ pontos)

      tabuleiroComPeca <- liftIO $ adicionarPecaAleatoria novoTabuleiro
      liftIO $ writeIORef estadoTabuleiro tabuleiroComPeca

      void $ element visualTabuleiro # set children []
      novaVisualizacao <- renderizarTabuleiro tabuleiroComPeca
      void $ element visualTabuleiro #+ [element novaVisualizacao]

      when (any (elem 2048) tabuleiroComPeca) $ mostrarAlerta janela "Você venceu!"
      when (fimDeJogo tabuleiroComPeca) $ mostrarAlerta janela "Game Over!"

      on UI.click botaoLogin $ \_ -> do 
        mostrarCaixaLogin janela ""

      on UI.click botaoCadastro $ \_ -> do 
        mostrarCaixaCadastro janela "" 

      novaPontuacao <- liftIO $ readIORef estadoPontuacao
      void $ element textoPontuacao # set UI.text ("Score: " ++ show novaPontuacao)

main :: IO ()
main = do
  tabuleiroInicial <- adicionarPecaAleatoria =<< adicionarPecaAleatoria tabuleiroVazio
  startGUI defaultConfig (configurarInterface tabuleiroInicial)