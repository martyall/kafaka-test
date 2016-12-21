{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries where

import Control.Error
import Control.Monad.Trans.Class (lift)
import Data.Monoid
import Data.String.Conv          (toS)
import Data.Text (Text)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Types

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------

(|?) :: Monad m => m [a] -> e -> ExceptT e m a
(|?) m err = lift m >>= \res -> (return $ listToMaybe res) !? err

infix 0 |?
--------------------------------------------------------------------------------
-- | User
--------------------------------------------------------------------------------

createUser :: Connection -> NewUser -> ExceptT AppError IO User
createUser conn nUsr = query conn q nUsr |? "makeUser: " <> toS nUsr
  where
    q = [sql|

          INSERT INTO users (username, email)
            VALUES ?
            RETURNING id, username, email;

            |]

readUser :: Connection -> UserId -> ExceptT AppError IO User
readUser conn uId = query conn q (Only uId) |? "readUser: " <> toS uId
  where
    q = [sql| SELECT * FROM users WHERE id = ?; |]

updateUser :: Connection -> User -> ExceptT AppError IO User
updateUser conn usr = query conn q usrData |? "updateUser: " <> toS usr
  where
    usrData = (userUsername usr, userEmail usr, userId usr)
    q = [sql| UPDATE users
                 SET username = ?, email = ?
                 WHERE id = ?;

            |]
