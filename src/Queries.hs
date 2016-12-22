{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Queries where

import Control.Error
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader
import Control.Lens
import Data.List                 (intersperse)
import Data.Monoid
import Data.String.Conv          (toS)
import Data.Text (Text)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Types

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------

(|?) :: MonadIO m =>  IO [a] -> Text -> ExceptT AppError m a
(|?) m err = liftIO m >>= \res -> (return $ listToMaybe res) !? NoResults err

infix 0 |?

showText :: Show a => a -> Text
showText = toS . show

liftPG :: HasPostgres m => (Connection -> b -> m a) -> b -> m a
liftPG f b = do
  conn <- getPostgresConnection
  f conn b

--------------------------------------------------------------------------------
-- | User
--------------------------------------------------------------------------------

createUser :: MonadIO m => Connection -> NewUser -> ExceptT AppError m User
createUser conn nUsr = query conn q nUsr |? "createUser: " <> showText nUsr
  where
    q = [sql|

          INSERT INTO users (username, email)
            VALUES ?
            RETURNING id, username, email;

            |]

readUser :: MonadIO m => Connection -> UserId -> ExceptT AppError m User
readUser conn uId = query conn q (Only uId) |? "readUser: " <> showText uId
  where
    q = [sql|

          SELECT id, username, email
            FROM users
            WHERE id = ?;

            |]

updateUser :: MonadIO m => Connection -> User -> ExceptT AppError m User
updateUser conn usr = query conn q usrData |? "updateUser: " <> showText usr
  where
    usrData = (userUsername usr, userEmail usr, userId usr)
    q = [sql|

          UPDATE users
            SET username = ?, email = ?
            WHERE id = ?
            RETURNING id, username, email;

            |]

deleteUser :: MonadIO m => Connection -> UserId -> ExceptT AppError m ()
deleteUser conn uId = do
    rowsAffected <- liftIO $ execute conn q (Only uId)
    if rowsAffected == 1
      then return ()
      else throwE $ NoResults ("deleteUser: " <> showText uId)
  where
    q = [sql|

          DELETE FROM users
            WHERE id = ?;

            |]

--------------------------------------------------------------------------------
-- | Media
--------------------------------------------------------------------------------

createMedia :: MonadIO m => Connection -> NewMedia -> ExceptT AppError m Media
createMedia conn nMed = query conn q nMed |? "createMedia: " <> showText nMed
  where
    q = [sql|

          INSERT INTO media (owner, caption, media_ref)
            VALUES ?
            RETURNING id, owner, caption, media_ref;

            |]

readMedia :: MonadIO m => Connection -> MediaId -> ExceptT AppError m Media
readMedia conn mId = query conn q (Only mId) |? "readMedia: " <> showText mId
  where
    q = [sql|

          SELECT id, owner, caption, media_ref
            FROM users
            WHERE id = ?;

            |]

updateMedia :: MonadIO m => Connection -> Media -> ExceptT AppError m Media
updateMedia conn med = query conn q mediaData |? "updateMedia: " <> showText med
  where
    mediaData = (mediaOwner med, mediaCaption med, mediaRef med, mediaId med)
    q = [sql|

          UPDATE media
            SET owner = ?, caption = ?, media_ref = ?
            WHERE id = ?
            RETURNING id, owner, caption, media_ref;

            |]

deleteMedia :: MonadIO m => Connection -> MediaId -> ExceptT AppError m ()
deleteMedia conn mId = do
    rowsAffected <- liftIO $ execute conn q (Only mId)
    if rowsAffected == 1
      then return ()
      else throwE $ NoResults ("deleteUser: " <> showText mId)
  where
    q = [sql|

          DELETE FROM media
            WHERE id = ?;

            |]
