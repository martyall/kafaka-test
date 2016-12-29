{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

module Queries where

import           Control.Arrow (returnA)
import           Control.Lens
import           Data.ByteString.Lazy      (ByteString)
import           Data.Int                  (Int64)
import           Data.List                 (intersperse)
import           Data.Maybe                (listToMaybe)
import           Data.String.Conv          (toS)

import           Database.PostgreSQL.Simple (Connection)
import           Opaleye                   (runInsertManyReturning, queryTable,
                                            restrict, (.==), Query, Column, runQuery,
                                            runUpdateReturning, runDelete)

import qualified Opaleye.PGTypes as P

import           Types

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------

maybeResult :: Functor f => f [a] -> f (Maybe a)
maybeResult = fmap listToMaybe

class Queryable (k :: CrudType) where
  createQuery :: Sing k -> Connection -> NewData k -> IO (Maybe (BaseData k))
  readQuery :: Sing k -> Connection -> ReadData k -> IO (Maybe (BaseData k))
  updateQuery :: Sing k -> Connection -> BaseData k -> IO (Maybe (BaseData k))
  deleteQuery :: Sing k -> Connection -> ReadData k -> IO Int64
--------------------------------------------------------------------------------
-- | User
--------------------------------------------------------------------------------

createUser :: Connection -> NewUser -> IO (Maybe User)
createUser conn nUsr =
  maybeResult $ runInsertManyReturning conn userTable [toPG nUsr] id

readUser :: Connection -> UserId -> IO (Maybe User)
readUser conn uId = maybeResult $ runQuery conn $ readUserQuery uId
  where
    readUserQuery :: UserId -> Query UserRow
    readUserQuery uId = proc () -> do
      row@(User' uId' _ _) <- queryTable userTable -< ()
      restrict -< pgUId uId .== uId'
      returnA -< row
    pgUId :: UserId -> Column P.PGInt4
    pgUId uId = P.pgInt4 $ uId ^. getUserId

updateUser :: Connection -> User -> IO (Maybe User)
updateUser conn usr =
  let uId' = P.pgInt4 $ usr ^. userId ^. getUserId
  in maybeResult $ runUpdateReturning conn userTable (const $ toPG usr)
    (\u -> u ^. userId .== uId') id

deleteUser :: Connection -> UserId -> IO Int64
deleteUser conn uId =
  let uId' = P.pgInt4 $ uId ^. getUserId
  in runDelete conn userTable (\u -> u ^. userId .== uId')

instance Queryable 'CrudUser where
  createQuery _ = createUser
  readQuery _ = readUser
  updateQuery _ = updateUser
  deleteQuery _ = deleteUser

--createUser conn nUsr = maybeResult $ runInsertManyReturning conn userTable [toPG nUsr] id
--
--createUser :: Connection
--           -> NewUser
--           -> IO (Maybe User)
--createUser conn nUsr = maybeResult $ runInsertManyReturning conn userTable [toPG nUsr] id
--
--createMedia :: Connection -> NewMedia -> Handler Media
--createMedia conn nMed = maybeResult (query conn q nMed) !? err500
--  where
--    q = [sql|
--
--          INSERT INTO media (owner, caption, media_ref)
--            VALUES ?
--            RETURNING id, owner, caption, media_ref;
--
--            |]


--readUser :: Connection -> UserId -> Handler User
--readUser conn uId = maybeResult (query conn q (Only uId)) !?
--    err404 {errBody = "user_not_found-" <> toBody uId}
--  where
--    q = [sql|
--
--          SELECT id, username, email
--            FROM users
--            WHERE id = ?;
--
--            |]
--
--updateUser :: Connection -> User -> Handler User
--updateUser conn usr = maybeResult (query conn q usrData) !?
--    err404 {errBody = "user_not_found-" <> (toBody . userId $ usr)}
--  where
--    usrData = (userUsername usr, userEmail usr, userId usr)
--    q = [sql|
--
--          UPDATE users
--            SET username = ?, email = ?
--            WHERE id = ?
--            RETURNING id, username, email;
--
--            |]
--
--deleteUser :: Connection -> UserId -> Handler ()
--deleteUser conn uId = do
--    rowsAffected <- liftIO $ execute conn q (Only uId)
--    if rowsAffected == 1
--      then return ()
--      else throwE $ err404 {errBody = "user_not_found-" <> toBody uId}
--  where
--    q = [sql|
--
--          DELETE FROM users
--            WHERE id = ?;
--
--            |]
--
----------------------------------------------------------------------------------
---- | Media
----------------------------------------------------------------------------------
--
--createMedia :: Connection -> NewMedia -> Handler Media
--createMedia conn nMed = maybeResult (query conn q nMed) !? err500
--  where
--    q = [sql|
--
--          INSERT INTO media (owner, caption, media_ref)
--            VALUES ?
--            RETURNING id, owner, caption, media_ref;
--
--            |]
--
--readMedia :: Connection -> MediaId -> Handler Media
--readMedia conn mId = maybeResult (query conn q (Only mId)) !?
--    err404 {errBody = "media_not_found-" <> toBody mId}
--  where
--    q = [sql|
--
--          SELECT id, owner, caption, media_ref
--            FROM users
--            WHERE id = ?;
--
--            |]
--
--updateMedia :: Connection -> Media -> Handler Media
--updateMedia conn med = maybeResult (query conn q mediaData) !?
--    err404 {errBody = "media_not_found-" <> (toBody . mediaId $ med)}
--  where
--    mediaData = (mediaOwner med, mediaCaption med, mediaRef med, mediaId med)
--    q = [sql|
--
--          UPDATE media
--            SET owner = ?, caption = ?, media_ref = ?
--            WHERE id = ?
--            RETURNING id, owner, caption, media_ref;
--
--            |]
--
--deleteMedia :: Connection -> MediaId -> Handler ()
--deleteMedia conn mId = do
--    rowsAffected <- liftIO $ execute conn q (Only mId)
--    if rowsAffected == 1
--      then return ()
--      else throwE $ err404 {errBody = "media_not_found-" <> toBody mId}
--  where
--    q = [sql|
--
--          DELETE FROM media
--            WHERE id = ?;
--
--            |]
