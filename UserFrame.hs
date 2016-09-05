{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module UserFrame where 

import qualified Control.Foldl as L
import qualified Data.Foldable as F
import Lens.Family (view, set, over)
import Frames
import Frames.CSV (defaultParser, readTableOpt) 
import Pipes hiding (Proxy) -- Producer
import Data.List (findIndex)
import qualified Data.Text as T

-- define a Show instance for frames
instance (Show a) => Show (Frame a) where
  show (Frame l f) = (show $ f 0) 
                       ++ (if l>1 then "\n" ++ (show $ f 1) else "")
                         ++ (if l>2 then "\n..." else "") 
                           ++ "\nFrame with " ++ (show l) ++ if(l>1) then " rows." else " row." 

-- create users Frame 

tableTypes "User" "Users.csv"
usersStream :: Producer User IO ()
usersStream = readTableOpt defaultParser "Users.csv"

loadUsers :: IO (Frame User)
loadUsers = inCoreAoS usersStream


-- function to get user password
getUserPassword :: Text -> IO (Maybe String)
getUserPassword uname = do
  users <- loadUsers
  let allUsers = F.toList (rget username <$> users)
  let index = findIndex (==uname) allUsers
  case index of 
    Nothing -> return Nothing
    Just i -> return $ Just (T.unpack ((F.toList (rget password <$> users)) !! i))

