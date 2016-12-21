{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module QueryAlgebra where

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Types


--------------------------------------------------------------------------------
-- CRUD query algebra and smart constructors
--------------------------------------------------------------------------------

data Crud s :: * where
  CreateOp :: Sing (k :: CrudType) -> NewData k -> Crud (BaseData k)
  ReadOp   :: Sing (k :: CrudType) -> ReadData k -> Crud (BaseData k)
  UpdateOp :: Sing (k :: CrudType) -> BaseData k -> Crud (BaseData k)
  DeleteOp :: Sing (k :: CrudType) -> ReadData k -> Crud ()

createOp :: Member Crud r
         => Sing (k :: CrudType)
         -> NewData k
         -> Eff r (BaseData k)
createOp s n = send $ CreateOp s n

readOp :: Member Crud r
         => Sing (k :: CrudType)
         -> ReadData k
         -> Eff r (BaseData k)
readOp s n = send $ ReadOp s n

updateOp :: Member Crud r
         => Sing (k :: CrudType)
         -> BaseData k
         -> Eff r (BaseData k)
updateOp s n = send $ UpdateOp s n

deleteOp :: Member Crud r
         => Sing (k :: CrudType)
         -> ReadData k
         -> Eff r ()
deleteOp s n = send $ DeleteOp s n
