{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module CRUD.Core where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Servant
import Data.Aeson (FromJSON)
import GHC.Generics
import Data.Proxy
import Control.Monad.IO.Class (liftIO)

import CRUD.Query

data NovoUsuario = NovoUsuario {
  nomeUsuario     :: String,
  senhaUsuario    :: String,
  pontuacaoUsuario :: Int
} deriving (Generic, FromJSON)

data RequisicaoLogin = RequisicaoLogin { 
  nomeLogin  :: String,
  senhaLogin :: String
} deriving (Generic, FromJSON)

type UsuarioAPI =
       "usuarios" :> Get '[JSON] [Usuario]
  :<|> "usuarios" :> ReqBody '[JSON] NovoUsuario :> Post '[JSON] ()
  :<|> "login"    :> ReqBody '[JSON] RequisicaoLogin :> Post '[JSON] Bool

servidor :: Server UsuarioAPI
servidor = buscarUsuarios
      :<|> inserirUsuario
      :<|> autenticarLogin

buscarUsuarios :: Handler [Usuario]
buscarUsuarios = liftIO buscarUsuariosQ

inserirUsuario :: NovoUsuario -> Handler ()
inserirUsuario (NovoUsuario nome senha pontos) = liftIO $ inserirUsuarioHash nome senha pontos

autenticarLogin :: RequisicaoLogin -> Handler Bool
autenticarLogin (RequisicaoLogin nome senha) = liftIO $ verificarLogin nome senha

aplicacao :: Application
aplicacao = serve (Proxy :: Proxy UsuarioAPI) servidor

servidorPrincipal :: IO ()
servidorPrincipal = run 8080 (cors (const $ Just politica) aplicacao)
  where
    politica = simpleCorsResourcePolicy
      { corsOrigins = Just (["http://127.0.0.1:8023"], True)
      , corsRequestHeaders = ["Content-Type"]
      , corsMethods = ["GET", "POST", "OPTIONS"]
      }