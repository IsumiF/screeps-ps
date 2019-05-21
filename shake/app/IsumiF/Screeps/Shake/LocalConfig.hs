{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module IsumiF.Screeps.Shake.LocalConfig
  ( LocalConfig(..)
  , localConfig_email
  , localConfig_password
  , localConfig_branch
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)

import           IsumiF.Screeps.Shake.Internal

data LocalConfig = LocalConfig
  { _localConfig_email    :: Text
  , _localConfig_password :: Text
  , _localConfig_branch   :: Text
  } deriving (Show, Eq, Generic)

makeLenses ''LocalConfig

instance FromJSON LocalConfig where
  parseJSON = genericParseJSON lensDefaultOptions
