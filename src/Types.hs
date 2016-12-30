{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
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


import Data.Aeson                           (FromJSON(..), ToJSON(..),
                                             object, (.=), (.:))
import Data.Aeson.Types                     (Value(..), typeMismatch)
import Data.Aeson.TH                        (deriveJSON, deriveFromJSON
                                            ,defaultOptions)
import Data.CaseInsensitive
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import Data.Profunctor.Product              (p3, p4)
import Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import Data.Text                            (Text)
import Data.Singletons                      (Sing)
import Data.Singletons.TH                   (genSingletons)
import Database.PostgreSQL.Simple           (Connection, ConnectInfo(..),
                                             connect)
import Database.PostgreSQL.Simple.Internal  (newNullConnection)
import Opaleye                              (Column, Table(Table),
                                             required, optional,
                                             PGInt4, PGText,
                                             PGCitext)
import Opaleye.Internal.RunQuery            (QueryRunnerColumnDefault(..))
import qualified Opaleye.PGTypes as P
import Servant                              (ServantErr)
import System.IO                            (FilePath)
import Web.HttpApiData                      (FromHttpApiData)

import Orphans                              ()

--------------------------------------------------------------------------------
-- | Environment
--------------------------------------------------------------------------------

data Environment =
    Development
  | Test
  deriving (Eq, Show, Read)


mkEnv :: C.Config -> IO Environment
mkEnv cfg = read <$> C.require cfg "Environment"

--------------------------------------------------------------------------------
-- | Misc
--------------------------------------------------------------------------------

type f ~> g = forall a. f a -> g a

type Handler = ExceptT ServantErr IO

--------------------------------------------------------------------------------
-- | CRUD types
--------------------------------------------------------------------------------

data CrudType = CrudUser
              | CrudMedia

genSingletons [''CrudType]

type family ReadData (c :: CrudType) :: *

type family BaseData (c :: CrudType) :: *

type family NewData (c :: CrudType) :: *

class PGEntity e where
  type WriteRow e :: *
  toPG :: e -> WriteRow e

class Example (k :: CrudType) where
  exampleId :: Sing k -> ReadData k
  exampleBase :: Sing k -> BaseData k
  exampleNew :: Sing k -> NewData k

--------------------------------------------------------------------------------
-- | User
--------------------------------------------------------------------------------

data NewUser = NewUser
  { _newUserUsername :: CI Text
  , _newUserEmail    :: CI Text
  } deriving (Eq, Show)

makeLenses ''NewUser

instance FromJSON NewUser where
  parseJSON (Object v) =
    NewUser <$> v .: "userUsername"
            <*> v .: "userEmail"
  parseJSON v = typeMismatch "NewUser" v

newtype UserId = UserId { _getUserId :: Int }
  deriving (Eq, Show, FromJSON, ToJSON, FromHttpApiData, QueryRunnerColumnDefault PGInt4)

makeLenses ''UserId

data User' a b c = User'
  { _userId       :: a
  , _userUsername :: b
  , _userEmail    :: c
  }

makeLenses ''User'

type User = User' UserId (CI Text) (CI Text)

-- $(deriveJSON defaultOptions ''User' UserId (CI Text) (CI Text))

instance FromJSON User where
  parseJSON (Object v) =
    User' <$> v .: "userId"
          <*> v .: "userUsername"
          <*> v .: "userEmail"
  parseJSON v = typeMismatch "User" v

instance ToJSON User where
  toJSON usr = object [ "userId" .= (usr ^. userId)
                      , "userUsername" .= (usr ^. userUsername)
                      , "userEmail" .= (usr ^. userEmail)
                      ]

type NewUserRow = User' (Maybe (Column PGInt4)) (Column PGCitext) (Column PGCitext)
type UserRow = User' (Column PGInt4) (Column PGCitext) (Column PGCitext)

$(makeAdaptorAndInstance "pUser" ''User')

instance PGEntity NewUser where
  type WriteRow NewUser = NewUserRow
  toPG nUsr = let name = P.pgCiStrictText $ nUsr ^. newUserUsername
                  email = P.pgCiStrictText $ nUsr ^. newUserEmail
              in User' Nothing name email

instance PGEntity User where
  type WriteRow User = NewUserRow
  toPG usr = let uId = Just $ P.pgInt4 $ usr ^. userId ^. getUserId
                 name = P.pgCiStrictText $ usr ^. userUsername
                 email = P.pgCiStrictText $ usr ^. userEmail
             in User' uId name email

instance Example 'CrudUser where
  exampleId _ = UserId 1
  exampleBase _ = User' (UserId 1) (mk "ali") (mk "ali@gmail.com")
  exampleNew _ = NewUser (mk "ali") (mk "ali@gmail.com")

userTable :: Table NewUserRow UserRow
userTable = Table "users" (pUser User' { _userId = optional "id"
                                       , _userUsername = required "username"
                                       , _userEmail = required "email" })

type instance ReadData 'CrudUser = UserId

type instance BaseData 'CrudUser = User

type instance NewData 'CrudUser = NewUser

----------------------------------------------------------------------------------
---- | Media
----------------------------------------------------------------------------------

--data NewMedia = NewMedia
--  { _newMediaOwner   :: UserId
--  , _newMediaCaption :: CI Text
--  , _newMediaRef     :: Text
--  }
--
--makeLenses ''NewMedia
---- $(deriveFromJSON defaultOptions ''NewMedia)
--
--newtype MediaId = MediaId {_getMediaId :: Int}
--  deriving (Eq, Show, FromJSON, ToJSON, FromHttpApiData, QueryRunnerColumnDefault PGInt4)
--
--makeLenses ''MediaId
--
--data Media' a b c d = Media'
--  { _mediaId      :: a
--  , _mediaOwner   :: b
--  , _mediaCaption :: c
--  , _mediaRef     :: d
--  }
--
--makeLenses ''Media'
---- $(deriveJSON defaultOptions ''Media')
--
  
--type Media = Media' MediaId UserId (CI Text) Text
--type NewMediaColumn = Media' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGCitext) (Column PGText)
--type MediaColumn = Media' (Column PGInt4) (Column PGInt4) (Column PGCitext) (Column PGText)
--
-- $(makeAdaptorAndInstance "pMedia" ''Media')
--
--mediaTable :: Table NewMediaColumn MediaColumn
--mediaTable = Table "media" (pMedia Media' { _mediaId =  optional "id"
--                                          , _mediaOwner = required "owner_id"
--                                          , _mediaCaption = required "caption"
--                                          , _mediaRef = required "ref" })
--
--type instance ReadData 'CrudMedia = MediaId
--
--type instance BaseData 'CrudMedia = Media
--
--type instance NewData 'CrudMedia = NewMedia

--instance PGEntity MediaCrud where
--  toPG nMed = let owner = P.pgInt4 $ nMed ^. newMediaOwner ^. getUserId
--                  cap = P.pgCiStrictText $ nMed ^. newMediaCaption
--                  ref = P.pgStrictText $ nMed ^. newMediaRef
--              in Media' Nothing owner cap ref
--
--instance PGEntity Media MediaColumn where
--  toPG med = let mId = P.pgInt4 $ med ^. mediaId ^. getMediaId
--                 owner = P.pgInt4 $ med ^. mediaOwner ^. getUserId
--                 cap = P.pgCiStrictText $ med ^. mediaCaption
--                 ref = P.pgStrictText $ med ^. mediaRef
--              in Media' mId owner cap ref
--------------------------------------------------------------------------------
-- | App Config
--------------------------------------------------------------------------------

data AppEnv = AppEnv
  { _pgConnection :: Connection
  , _appEnv :: Environment
  }

makeLenses ''AppEnv

mkPG :: C.Config -> IO Connection
mkPG cfg = do
  host <- C.lookup cfg "host"
  port <- C.lookup cfg "port"
  user <- C.lookup cfg "user"
  pwd <- C.lookup cfg "password"
  db <-  C.lookup cfg "db"
  let info = ConnectInfo <$> host <*> port <*> user <*> pwd <*> db
  case info of
    Nothing -> newNullConnection
    Just info -> connect info

mkAppEnv :: FilePath -> IO AppEnv
mkAppEnv fp = do
  conf <- C.load [ C.Required fp ]
  appE <- mkEnv $ C.subconfig "env" conf
  pgConn <- mkPG $ C.subconfig "pg" conf
  return $ AppEnv pgConn appE
