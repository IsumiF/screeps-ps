{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module IsumiF.Screeps.Shake.Commit
  ( uploadMainModule
  ) where

import           Control.Monad.IO.Class
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString               as BS
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Proxy                    (Proxy (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           GHC.Generics                  (Generic)
import           Network.HTTP.Client           (newManager)
import           Network.HTTP.Client.TLS       (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           System.FilePath               (takeBaseName)

import           IsumiF.Screeps.Shake.Internal

uploadMainModule :: MonadIO m
                 => Text -- ^email
                 -> Text -- ^password
                 -> Text -- ^ branch name
                 -> FilePath -- ^main module path
                 -> m ()
uploadMainModule email password branch path = do
    httpManager <- liftIO $ newManager tlsManagerSettings
    let clientEnv = mkClientEnv httpManager baseUrl'

    moduleContent <- liftIO $ T.decodeUtf8 <$> BS.readFile path
    let moduleName = takeBaseName . takeBaseName $ path
        reqBody = CommitCodeReqBody branch (Map.singleton (T.pack moduleName) moduleContent)
        authData = BasicAuthData
          (T.encodeUtf8 email)
          (T.encodeUtf8 password)

    result <- liftIO $ runClientM (commitCode' reqBody authData) clientEnv
    case result of
      Left err -> error $ show err
      Right _  -> pure ()

baseUrl' :: BaseUrl
baseUrl' = BaseUrl Https "screeps.com" 443 ""

commitCode' :: CommitCodeReqBody -> BasicAuthData -> ClientM Aeson.Value
commitCode' = client (Proxy :: Proxy ApiCommitCode)

type ApiCommitCode = "api" :> "user" :> "code"
  :> ReqBody '[JSON] CommitCodeReqBody
  :> BasicAuth "screeps" ScreepsUser
  :> Post '[JSON] Aeson.Value

data CommitCodeReqBody = CommitCodeReqBody
  { _commitCodeReqBody_branch  :: Text
  , _commitCodeReqBody_modules :: Map Text Text
  } deriving (Show, Eq, Generic)

data ScreepsUser = ScreepsUser

instance Aeson.ToJSON CommitCodeReqBody where
  toJSON = Aeson.genericToJSON lensDefaultOptions
  toEncoding = Aeson.genericToEncoding lensDefaultOptions
