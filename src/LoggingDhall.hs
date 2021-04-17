{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{- |
Module      : LoggingDhall
Description : Utility methods
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3

Mainly has various logging functions and timing of commands.
Allows you to log to a file or the screen or both
-}
module LoggingDhall where
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Dhall
    ( defaultInterpretOptions,
      genericAutoWith,
      genericToDhallWith,
      input,
      Decoder,
      FromDhall(..),
      InterpretOptions(fieldModifier),
      ToDhall(..) )
import qualified Dhall as D (auto)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Logging

deriving instance FromDhall LLog
deriving instance ToDhall LLog

deriving instance FromDhall ScreenType
deriving instance ToDhall ScreenType

instance FromDhall Email where
  autoWith _i = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop (T.length "e") }

instance ToDhall Email where
  injectWith _o = genericToDhallWith defaultInterpretOptions { fieldModifier = T.drop (T.length "e") }

instance FromDhall File where
  autoWith _i = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop (T.length "f") }

instance ToDhall File where
  injectWith _o = genericToDhallWith defaultInterpretOptions { fieldModifier = T.drop (T.length "f") }

instance FromDhall LogOpts where
  autoWith _i = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop (T.length "l") }

logopts :: Decoder LogOpts
logopts =  genericAutoWith defaultInterpretOptions { fieldModifier = T.drop (T.length "l") }

instance ToDhall LogOpts where
  injectWith _o = genericToDhallWith defaultInterpretOptions { fieldModifier = T.drop (T.length "l") }

instance FromDhall Screen where
  autoWith _i = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop (T.length "s") }

instance ToDhall Screen where
  injectWith _o = genericToDhallWith defaultInterpretOptions { fieldModifier = T.drop (T.length "s") }

loadDhallTH :: forall a . (TH.Lift a, FromDhall a) => Text -> TH.Q TH.Exp
loadDhallTH txt = do
  c <- TH.runIO $ input (D.auto @a) txt
  TH.lift c

loadFromLogConfig :: Text -> IO LogOpts
loadFromLogConfig expr = do
  config <- input D.auto expr :: IO LogOpts
  T.putStrLn $ "configuration [" <> expr <> "] found:" <> T.pack (show config)
  return config

leWith :: MonadUnliftIO m => Text -> e -> (LogOpts -> LogOpts) -> RL e m a -> m a
leWith expr e g ma = do
  logcfg <- liftIO $ loadFromLogConfig expr
  logWith e (g logcfg) ma

fbeWith :: MonadUnliftIO m => e -> (LogOpts -> LogOpts) -> RL e m a -> m a
fbeWith = leWith "./log.dhall" -- batch stuff

fb :: MonadUnliftIO m => RL () m a -> m a
fb = fbeWith () id -- batch

