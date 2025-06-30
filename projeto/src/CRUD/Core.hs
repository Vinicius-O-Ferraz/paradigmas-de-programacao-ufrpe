-- File: src/CRUD/Core.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module CRUD.Core where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Servant
import Data.Aeson (FromJSON, ToJSON(..), object, (.=))
import GHC.Generics
import Data.Proxy
import Control.Monad.IO.Class (liftIO)

import CRUD.Query

data NewUser = NewUser {
  userName     :: String,
  userPassword :: String,
  userScore :: Int
} deriving (Generic, FromJSON)

data LoginRequest = LoginRequest {
  loginName     :: String,
  loginPassword :: String
} deriving (Generic, FromJSON)

data Leaderboard = Leaderboard { players :: [User] }
  deriving (Generic, FromJSON)

instance ToJSON Leaderboard where
  toJSON (Leaderboard ps) = object ["players" .= ps]

type UserAPI =
       "users" :> Get '[JSON] [User]
  :<|> "users" :> ReqBody '[JSON] NewUser :> Post '[JSON] ()
  :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Bool
  :<|> "leaderboard" :> Get '[JSON] Leaderboard

fetchTopUsers :: Handler Leaderboard
fetchTopUsers = do
  userList <- liftIO fetchTopUsersQ
  liftIO $ putStrLn "\n---[ DEBUG PÓDIO ACIONADO ]---"
  liftIO $ putStrLn "Top 5 usuários retornados do banco de dados:"
  liftIO $ mapM_ printUser userList
  liftIO $ putStrLn "---------------------------------\n"
  return $ Leaderboard { players = userList }
  where
    printUser (User _ name _ score) = putStrLn $ "  -> Nome: " ++ name ++ ", Recorde: " ++ show score

server :: Server UserAPI
server = fetchUser
    :<|> insertUser
    :<|> login
    :<|> fetchTopUsers

fetchUser :: Handler [User]
fetchUser = liftIO fetchUserQ

insertUser :: NewUser -> Handler ()
insertUser (NewUser name pass score) = liftIO $ insertUserHashed name pass score

login :: LoginRequest -> Handler Bool
login(LoginRequest name pass) = do
  liftIO $ checkLogin name pass

app :: Application
app = serve (Proxy :: Proxy UserAPI) server

mainServer :: IO ()
mainServer = run 8080 (cors (const $ Just policy) app)
  where
    policy = simpleCorsResourcePolicy
      { corsOrigins = Just (["http://127.0.0.1:8023"], True)
      , corsRequestHeaders = ["Content-Type"]
      , corsMethods = ["GET", "POST", "OPTIONS"]
      }