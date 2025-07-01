{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CRUD.Query where

import Database.SQLite.Simple
import GHC.Generics
import Crypto.BCrypt (validatePassword, hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import qualified Data.ByteString.Char8 as BS
import Data.Aeson (ToJSON, FromJSON)

data Usuario = Usuario {
  idUsuario       :: Int,
  nomeUsuario     :: String,
  senhaUsuario    :: String,
  pontuacaoUsuario :: Int
} deriving (Eq, Show, FromRow, ToRow, Generic, ToJSON, FromJSON)

obterConexao :: IO Connection 
obterConexao = open "servantDB"

buscarUsuariosQ :: IO [Usuario]
buscarUsuariosQ = do
  conn <- obterConexao
  listaUsuarios <- query_ conn "select * from users;"
  close conn
  pure listaUsuarios

inserirUsuarioQ :: (String, String, Int) -> IO ()
inserirUsuarioQ usuario = do
  conn <- obterConexao
  execute conn "insert into users (user_name, user_password, user_score) values (?,?,?)" usuario
  close conn

inserirUsuarioHash :: String -> String -> Int -> IO ()
inserirUsuarioHash nome senha pontos = do
  let senhaBS = BS.pack senha
  talvezHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy senhaBS
  case talvezHash of
    Nothing   -> putStrLn "Erro ao gerar hash da senha!"
    Just hash -> inserirUsuarioQ (nome, BS.unpack hash, pontos)

verificarLogin :: String -> String -> IO Bool
verificarLogin nome senha = do
  conn <- obterConexao
  resultado <- query conn "SELECT user_password FROM users WHERE user_name = ?" (Only nome) :: IO [Only String]
  close conn
  case resultado of
    [Only hash] -> return $ validatePassword (BS.pack hash) (BS.pack senha)
    _           -> return False

teste :: IO ()
teste = putStrLn "Olá do módulo Consulta"
