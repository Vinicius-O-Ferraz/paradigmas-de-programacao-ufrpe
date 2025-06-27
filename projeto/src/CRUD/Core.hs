{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module CRUD.Core where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Aeson (FromJSON)
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

type UserAPI =
       "users" :> Get '[JSON] [User]
  :<|> "users" :> ReqBody '[JSON] NewUser :> Post '[JSON] ()
  :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Bool

server :: Server UserAPI
server = fetchUser
    :<|> insertUser
    :<|> login

fetchUser :: Handler [User]
fetchUser = liftIO fetchUserQ

insertUser :: NewUser -> Handler ()
insertUser (NewUser name pass score) = liftIO $ insertUserHashed name pass score

login :: LoginRequest -> Handler Bool
login(LoginRequest name pass) = do
  liftIO $ checkLogin name pass

app :: Application
app = serve (Proxy :: Proxy UserAPI) server

main :: IO ()
main = run 8080 app