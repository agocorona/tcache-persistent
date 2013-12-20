-----------------------------------------------------------------------------
--
-- Module      :  Data.TCache.Persistent
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}


module Data.TCache.Persistent.Sqlite(
module Database.Persist.Sqlite,
runSQL,
setPersistConfig
)  where

import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Types
import           Database.Persist.Sqlite
import           Data.TCache
import           Data.TCache.IndexQuery
import           Data.Typeable
import           System.IO.Unsafe
import           Data.Conduit
import           Control.Monad.Logger

import           System.IO.Unsafe
import           Data.IORef
import           Control.Monad.IO.Class

deriving instance  Typeable1 Entity

pool = unsafePerformIO $ do
    mconf <- readIORef _persistConf
    case mconf of
      Nothing -> error "persist config not set. use setPersitConfig"
      Just conf ->  createPoolConfig conf

runSQL sql= liftIO $ do
    mconf <- readIORef _persistConf
    case mconf of
      Nothing -> error "persist config not set. use setPersitConfig"
      Just conf -> runStdoutLoggingT $ runResourceT $ runPool conf sql pool

_persistConf :: IORef (Maybe SqliteConf)
_persistConf= unsafePerformIO $ newIORef Nothing

setPersistConfig x= writeIORef _persistConf $ Just x

instance (PersistEntity a,PersistEntityBackend a ~ SqlBackend)
  => IResource (Entity a) where

  keyResource Entity{entityKey= Key (PersistInt64 x)}= show x

  writeResource Entity{..}= runSQL $ insertKey entityKey entityVal

  delResource Entity{..}= runSQL $ delete entityKey

  readResourceByKey  k = do
      let ik= read k
      mr <- runSQL $ get $ Key (PersistInt64 ik)
      case mr of
        Nothing   -> return Nothing
        Just post -> return . Just $ Entity (Key $ PersistInt64 ik) post



deriving instance Typeable SqlBackend
deriving instance Typeable2 KeyBackend


