{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module QueryAlgebra where

import Data.Functor.Identity
import Database.PostgreSQL.Simple (Connection)

import Control.Error
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Internal

import Queries
import Types

--------------------------------------------------------------------------------
-- CRUD query algebra and smart constructors
--------------------------------------------------------------------------------

data Crud k s :: * where
  CreateOp :: Sing (k :: CrudType) -> NewData k -> Crud k (BaseData k)
  ReadOp   :: Sing (k :: CrudType) -> ReadData k -> Crud k (BaseData k)
  UpdateOp :: Sing (k :: CrudType) -> BaseData k -> Crud k (BaseData k)
  DeleteOp :: Sing (k :: CrudType) -> ReadData k -> Crud k ()

createOp :: Member (Crud k) r
         => Sing (k :: CrudType)
         -> NewData k
         -> Eff r (BaseData k)
createOp s n = send $ CreateOp s n

readOp :: Member (Crud k) r
         => Sing (k :: CrudType)
         -> ReadData k
         -> Eff r (BaseData k)
readOp s n = send $ ReadOp s n

updateOp :: Member (Crud k) r
         => Sing (k :: CrudType)
         -> BaseData k
         -> Eff r (BaseData k)
updateOp s n = send $ UpdateOp s n

deleteOp :: Member (Crud k) r
         => Sing (k :: CrudType)
         -> ReadData k
         -> Eff r ()
deleteOp s n = send $ DeleteOp s n

-- | Impure interpreters

runUser :: Member Handler r
        => Eff (Crud 'CrudUser ': r) a
        -> Eff r a
runUser = runNat runUser'
  where
    runUser' :: Crud 'CrudUser a -> Handler a
    runUser' c = case c of
      (CreateOp _ nUsr) -> (liftPG createUser) nUsr
      (ReadOp _ uId) -> (liftPG readUser) uId
      (UpdateOp _ usr) -> (liftPG updateUser) usr
      (DeleteOp _ uId) -> (liftPG deleteUser) uId

runMedia :: Member Handler r
         => Eff (Crud 'CrudMedia ': r) a
         -> Eff r a
runMedia = runNat runMedia'
  where
    runMedia' :: Crud 'CrudMedia a -> Handler a
    runMedia' c = case c of
      (CreateOp _ nUsr) -> (liftPG createMedia) nUsr
      (ReadOp _ uId) -> (liftPG readMedia) uId
      (UpdateOp _ usr) -> (liftPG updateMedia) usr
      (DeleteOp _ uId) -> (liftPG deleteMedia) uId

-- | PureInterpreters

--aliNew :: NewUser
--aliNew = NewUser "ali-babba" "ali@gmail.com"

ali :: User
ali = User (UserId 1) "ali-babba" "ali@gmail.com"

--photoNew :: NewMedia
--photoNew = NewMedia (UserId 1) "jin" "MEDIAREF"

photo :: Media
photo = Media (MediaId 1) (UserId 1) "jin" "MEDIAREF"

runUserPure :: Member Identity r
            => Eff (Crud 'CrudUser ': r) a
            -> Eff r a
runUserPure = runNat runUserPure'
  where
    runUserPure' :: Crud 'CrudUser a -> Identity a
    runUserPure' c = case c of
      (CreateOp _ _) -> return ali
      (ReadOp _ _) -> return ali
      (UpdateOp _ _) -> return ali
      (DeleteOp _ _) -> return ()

runMediaPure :: Member Identity r
             => Eff (Crud 'CrudMedia ': r) a
             -> Eff r a
runMediaPure = runNat runMediaPure'
  where
    runMediaPure' :: Crud 'CrudMedia a -> Identity a
    runMediaPure' c = case c of
      (CreateOp _ _) -> return photo
      (ReadOp _ _) -> return photo
      (UpdateOp _ _) -> return photo
      (DeleteOp _ _) -> return ()
