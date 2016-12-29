{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QueryAlgebra
  ( createOp
  , readOp
  , updateOp
  , deleteOp
  , interpretCrud
  ) where

import           Data.Functor.Identity      (Identity(..))
import           Database.PostgreSQL.Simple (Connection)

import           Control.Error
import           Control.Lens               ((^.))
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import qualified Control.Monad.Reader as R
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.Internal

import           Servant (ServantErr, err404)

import           Queries
import           Types

--------------------------------------------------------------------------------
-- CRUD query algebra and smart constructors
--------------------------------------------------------------------------------

data Crud k s :: * where
  CreateOp :: Sing (k :: CrudType) -> NewData k -> Crud k (BaseData k)
  ReadOp   :: Sing (k :: CrudType) -> ReadData k -> Crud k (BaseData k)
  UpdateOp :: Sing (k :: CrudType) -> BaseData k -> Crud k (BaseData k)
  DeleteOp :: Sing (k :: CrudType) -> ReadData k -> Crud k ()

createOp :: Sing (k :: CrudType)
         -> NewData k
         -> Eff '[Crud k] (BaseData k)
createOp s n = send $ CreateOp s n

readOp :: Sing (k :: CrudType)
         -> ReadData k
         -> Eff '[Crud k] (BaseData k)
readOp s n = send $ ReadOp s n

updateOp :: Sing (k :: CrudType)
         -> BaseData k
         -> Eff '[Crud k] (BaseData k)
updateOp s n = send $ UpdateOp s n

deleteOp :: Sing (k :: CrudType)
         -> ReadData k
         -> Eff '[Crud k] ()
deleteOp s n = send $ DeleteOp s n

-- | Impure interpreters

runImpure :: Queryable k
        => Eff '[Crud k] ~> Eff '[Reader AppEnv, Exc ServantErr, IO]
runImpure (Val a) = return a
runImpure (E u q) = do
  (e :: AppEnv) <- ask
  let conn = e ^. pgConnection
  case extract u of
    (CreateOp s newA) -> do
      mA <- send $ (createQuery s) conn newA
      a <- maybe (throwError err404) return mA
      runImpure $ qApp q a
    (ReadOp s aId) -> do
      mA <- send $ (readQuery s) conn aId
      a <- maybe (throwError err404) return mA
      runImpure $ qApp q a
    (UpdateOp s a) -> do
      mA <- send $ (updateQuery s) conn a
      a' <- maybe (throwError err404) return mA
      runImpure $ qApp q a'
    (DeleteOp s aId) -> do
      _ <- send $ (deleteQuery s) conn aId
      runImpure $ qApp q ()

interpretPG :: Eff '[Reader AppEnv, Exc ServantErr, IO] ~> R.ReaderT AppEnv Handler
interpretPG m = R.ReaderT $ \e -> ExceptT $ runM $ runError $ runReader m e


---- | PureInterpreters

runPure :: Example k
        => Eff '[Crud k] ~> Eff '[Identity]
runPure (Val a) = return a
runPure (E u q) = case extract u of
  (CreateOp s _) -> do
    a <- send . Identity $ exampleBase s
    runPure $ qApp q a
  (ReadOp s _) -> do
    a <- send . Identity $ exampleBase s
    runPure $ qApp q a
  (UpdateOp s _) -> do
    a <- send . Identity $ exampleBase s
    runPure $ qApp q a
  (DeleteOp s _) -> runPure $ qApp q ()

interpretPure :: Eff '[Identity] ~> R.ReaderT AppEnv Handler
interpretPure m = (return . runIdentity . runM $ m)

interpretCrud :: (Example k, Queryable k)
              => Eff '[Crud k] ~> R.ReaderT AppEnv Handler
interpretCrud m = do
  e <- R.ask
  case e ^. appEnv of
    Development -> interpretPG . runImpure $ m
    Test -> interpretPure . runPure $ m
