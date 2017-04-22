{-# LANGUAGE OverloadedStrings #-}
module Control.Config where

import Prelude hiding (lines, readFile, drop)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Environment (lookupEnv)


type Name = String
type Default = String
data Env = Env Name Default

loadEnv :: MonadIO m => Env -> m String
loadEnv (Env n d) = do
    mv <- liftIO $ lookupEnv n
    pure $ fromMaybe d mv


