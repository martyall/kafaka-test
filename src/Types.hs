{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Aeson
import Data.Aeson.TH                        (deriveJSON, deriveFromJSON
                                            ,defaultOptions)
import Data.Text                            (Text)
import Data.Singletons.TH                   (genSingletons)

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
  }

$(deriveJSON defaultOptions ''NewUser)

instance ToRow NewUser where
  toRow (NewUser nuName nuEmail) = toRow (nuName, nuEmail)

newtype UserId = UserId Integer
  deriving (Eq, Show, FromJSON, ToJSON, FromField, ToField)

data User = User
  { userId       :: UserId
  , userUsername :: UserId
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
