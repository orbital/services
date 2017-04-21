{-# LANGUAGE OverloadedStrings #-}
module Control.Config where

import Prelude hiding (lines, readFile, drop)
import Data.Map (Map, fromList, union)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (breakOn, unpack, lines, dropAround, drop)
import Data.Text.IO (readFile)
import qualified System.Environment as System


type Environment = Map String String


loadEnvSystem :: (MonadIO m) => m Environment
loadEnvSystem = fromList <$> (liftIO $ System.getEnvironment)


loadEnvFile :: (MonadIO m) => FilePath -> m Environment
loadEnvFile = fmap (fromList . parseEnv) . liftIO . readFile
  where
    parseEnv = map (strings . breakOn "=") . lines
    strings (a, b) = (unpack a, unpack $ stripQuotes $ drop 1 b)
    stripQuotes = dropAround (== '"')


loadEnv :: MonadIO m => FilePath -> m Environment
loadEnv p = do
    s <- loadEnvSystem
    f <- loadEnvFile p
    pure $ union s f


