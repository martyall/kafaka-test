{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Error                        (ExceptT)
import Control.Lens                         ((^.))
import Control.Lens.TH                      (makeLenses)
import Control.Monad.Reader                 (ask)
import Control.Monad.Trans                  (lift, MonadIO(..))
import Control.Monad.Trans.Reader           (ReaderT)


import Data.Aeson                           (FromJSON, ToJSON)
import Data.Aeson.TH                        (deriveJSON, deriveFromJSON
                                            ,defaultOptions)
import Data.Text                            (Text)
import Data.Singletons.TH                   (genSingletons)

import Database.PostgreSQL.Simple           (Connection)
import Database.PostgreSQL.Simple.FromRow   (FromRow(..), field)
import Database.PostgreSQL.Simple.ToRow     (ToRow(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.Types     (Only(..))

--------------------------------------------------------------------------------
-- | CRUD types
--------------------------------------------------------------------------------

data CrudType = CrudUser
              | CrudMedia

genSingletons [''CrudType]

type family ReadData (c :: CrudType) :: *

type family BaseData (c :: CrudType) :: *

type family NewData (c :: CrudType) :: *

--------------------------------------------------------------------------------
-- | User
--------------------------------------------------------------------------------

data NewUser = NewUser
  { newUserUsername :: Text
  , newUserEmail    :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''NewUser)

instance ToRow NewUser where
  toRow (NewUser nuName nuEmail) = toRow (nuName, nuEmail)

newtype UserId = UserId Integer
  deriving (Eq, Show, FromJSON, ToJSON, FromField, ToField)

data User = User
  { userId       :: UserId
  , userUsername :: Text
  , userEmail    :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToRow User where
  toRow (User uId uName uEmail) = toRow (uId, uName, uEmail)

type instance ReadData 'CrudUser = UserId

type instance BaseData 'CrudUser = User

type instance NewData 'CrudUser = NewUser

----------------------------------------------------------------------------------
---- | Media
----------------------------------------------------------------------------------

data NewMedia = NewMedia
  { newMediaOwner   :: UserId
  , newMediaCaption :: Text
  , newMediaRef     :: Text
  } deriving (Eq, Show)

$(deriveFromJSON defaultOptions ''NewMedia)

instance ToRow NewMedia where
  toRow (NewMedia mOId mCap mRef) = toRow (mOId, mCap, mRef)

newtype MediaId = MediaId Integer
  deriving (Eq, Show, FromJSON, ToJSON, FromField, ToField)

data Media = Media
  { mediaId      :: MediaId
  , mediaOwner   :: UserId
  , mediaCaption :: Text
  , mediaRef     :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Media)

instance FromRow Media where
  fromRow = Media <$> field <*> field <*> field <*> field

instance ToRow Media where
  toRow (Media mId mOId mCap mRef) = toRow (mId, mOId, mCap, mRef)

type instance ReadData 'CrudMedia = MediaId

type instance BaseData 'CrudMedia = Media

type instance NewData 'CrudMedia = NewMedia

--------------------------------------------------------------------------------
-- | Error Types
--------------------------------------------------------------------------------

data AppError = NoResults Text

--------------------------------------------------------------------------------
-- | Handler
--------------------------------------------------------------------------------

data AppEnv = AppEnv
  { _pg :: Connection }

makeLenses ''AppEnv

type Handler = ExceptT AppError (ReaderT AppEnv IO)

class MonadIO m => HasPostgres m where
  getPostgresConnection :: m Connection

instance MonadIO m => HasPostgres (ReaderT AppEnv m) where
  getPostgresConnection = do
    appEnv <- ask
    return $ appEnv ^. pg

instance HasPostgres m => HasPostgres (ExceptT e m) where
  getPostgresConnection = lift getPostgresConnection
