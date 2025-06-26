{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CRUD.Query where
import Database.SQLite.Simple
import GHC.Generics
import GHC.Generics (Constructor(conName))
import Crypto.BCrypt (validatePassword)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import qualified Data.ByteString.Char8 as BS

data User = User {
  userId:: Int,
  userName:: String,
  userPassword :: String
} deriving (Eq, Show, FromRow, ToRow, Generic)

getConn :: IO Connection 
getConn = open "servantDB"

fetchUserQ :: IO [User] 
fetchUserQ = do
  conn <- getConn
  userList <- query_ conn "select * from users;"
  close conn
  pure userList

insertUserQ :: User -> IO ()
insertUserQ user = do
  conn <- getConn
  execute conn execute conn "insert into users (user_name, user_password) values (?,?)" (name, pass)
  close conn

insertUserHashed :: String -> String -> IO ()
insertUserHashed name pass = do
  let passBS = BS.pack pass
  maybeHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy passBS
  case maybeHash of
    Nothing   -> putStrLn "Erro ao gerar hash da senha!"
    Just hash -> insertUserQ (name, BS.unpack hash)

checkLogin :: String -> String -> IO Bool
checkLogin name pass = do
  conn <- getConn
  result <- query conn "SELECT user_password FROM users WHERE user_name = ?" (Only name) :: IO [Only String]
  close conn
  case result of
    [Only hash] -> return $ validatePassword (BS.pack hash) (BS.pack pass)
    _           -> return False

test :: IO()
test = putStrLn "Hello from Query"