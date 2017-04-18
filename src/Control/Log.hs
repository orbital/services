{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Log
  ( message
  , value
  , error
  , runLogger
  , runLoggerOff
  , MonadLogger
  , bs
  , lbs
  , sanitize
  ) where

-- import Control.Lens ((^.))
import Prelude hiding (error)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runStdoutLoggingT, runNoLoggingT, LoggingT, NoLoggingT, MonadLogger, logInfoNS)
import Data.Monoid ((<>))
import Data.Text (Text, pack, replace)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT

type Section = Text

message :: MonadLogger m => Section -> Text -> m ()
message = logInfoNS

value :: (Show a) => a -> Text
value = pack . show

bs :: ByteString -> Text
bs = decodeUtf8

lbs :: BL.ByteString -> Text
lbs = LT.toStrict . LT.decodeUtf8

sanitize :: Text -> BL.ByteString -> Text
sanitize n h = replace n "xxx" $ lbs h

error :: MonadLogger m => Section -> Text -> m ()
error s t =
    message ("ERROR | " <> s) t

runLogger :: MonadIO m => LoggingT m a -> m a
runLogger = runStdoutLoggingT

runLoggerOff :: NoLoggingT m a -> m a
runLoggerOff = runNoLoggingT
