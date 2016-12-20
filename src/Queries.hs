{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKindis      #-}
{-# LANGUAGE TypeFamilies    #-}

module QueryAlgebra where

import Database.PostgreSQL.Simple.SqlQQ
import Data.Singletons
import Data.Singletons.TH

data CrudType = CrudUser
              | CrudMedia

$(genSingletons ''CrudType)

type family Object 

data CrudF next :: * where
  CreateOp :: Sing (k :: CrudType) -> Creatable k -> (Object k -> next)
  ReadOp   :: Sing (k :: CrudType) -> Readable k -> (Object k -> next)
  UpdateOp :: Sing (k :: CrudType) -> Updatable k -> (Object k -> next)
  DeleteOp :: Sing (k :: CrudType) -> Deletable k -> (() -> next)

