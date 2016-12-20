{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import Data.Aeson
import Data.Aeson.TH                        (deriveJSON, defaultOptions)
import Data.Text                            (Text)
import Data.Singletons                      (Sing(..), SingI(..))
import Data.Singletons.TH                   (genSingletons)
import Data.Vinyl.Core                      (Rec(..))
import Data.Vinyl.Lens
import Control.Lens                  hiding (Identity)
import Control.Lens.TH

import Database.PostgreSQL.Simple.FromRow   (FromRow(..), field)
import Database.PostgreSQL.Simple.ToRow     (ToRow(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.Types     (Only(..))

import GHC.TypeLits
--------------------------------------------------------------------------------
-- | Fields
--------------------------------------------------------------------------------

type family ElF (f :: Symbol) :: *

newtype Attr f = Attr { _unAttr :: ElF f }
makeLenses ''Attr

instance ToField (ElF f) => ToField (Attr f) where
  toField = toField . _unAttr

(=::) :: KnownSymbol f => Sing f -> ElF f -> Attr f
_ =:: x = Attr x

--------------------------------------------------------------------------------
-- | User
--------------------------------------------------------------------------------

newtype UserId = UserId Integer
  deriving (Eq, Show, FromJSON, ToJSON, FromField, ToField)

type instance ElF "personId" = UserId
type instance ElF "username" = Text
type instance ElF "email" = Text

type NewUser = '["username", "email"]
type User = '["personId", "username", "email"]

instance ToRow (Rec Attr NewUser) where
  toRow usr = toRow
    ( usr ^. rlens (sing :: Sing "username")
    , usr ^. rlens (sing :: Sing "email")
    )

instance FromRow (Rec Attr User) where
  fromRow = do
    uId <- field
    uName <- field
    uEmail <- field
    return $ ((sing :: Sing "personId") =:: uId)
          :& ((sing :: Sing "username") =:: uName)
          :& ((sing :: Sing "email")    =:: uEmail)
          :& RNil

instance ToRow (Rec Attr User) where
  toRow usr = toRow
    ( usr ^. rlens (sing :: Sing "personId")
    , usr ^. rlens (sing :: Sing "username")
    , usr ^. rlens (sing :: Sing "email")
    )

----------------------------------------------------------------------------------
---- | Media
----------------------------------------------------------------------------------
--
--newtype MediaId = MediaId Integer
--  deriving (Eq, Show, FromJSON, ToJSON, FromField, ToField)
--
--data Media = Media
--  { mediaId      :: MediaId
--  , mediaOwner   :: UserId
--  , mediaCaption :: Text
--  } deriving (Eq, Show)
--
-- $(deriveJSON defaultOptions ''Media)
--
--instance FromRow Media where
--  fromRow = Media <$> field <*> field <*> field
--
--instance ToRow Media where
--  toRow (Media mId mOId mCap) = toRow (mId, mOId, mCap)
