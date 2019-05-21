module Main where

import           Control.Lens
import qualified Data.Aeson                       as Aeson
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Text                        as T
import           Development.Shake
import           Development.Shake.FilePath
import           IsumiF.Screeps.Shake.Commit      (uploadMainModule)
import           System.Process                   (callCommand)

import           IsumiF.Screeps.Shake.LocalConfig

buildDir :: FilePath
buildDir = "build"

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = buildDir } $ do
    want [ buildDir </> "main.min.js" ]

    config <- liftIO $ readLocalConfig

    phony "clean" $ do
      putNormal "Cleaning files in './build'"
      removeFilesAfter buildDir ["//*"]

    let makeDeployPhony taskName mainModule =
          phony taskName $ do
            need [buildDir </> mainModule]
            putNormal $ "Deploying to branch " <> T.unpack (config ^. localConfig_branch)
            uploadMainModule
              (config ^. localConfig_email)
              (config ^. localConfig_password)
              (config ^. localConfig_branch)
              (buildDir </> mainModule)

    makeDeployPhony "deploy" "main.min.js"
    makeDeployPhony "deploy-dev" "main.js"

    phony "repl" $ liftIO $ callCommand "pulp repl"

    phony "build" $ need [buildDir </> "main.min.js"]
    phony "build-dev" $ need [buildDir </> "main.js"]

    buildDir </> "main.min.js" %> \out -> do
      need [buildDir </> "main.js"]
      liftIO $ callCommand $
        "pulp browserify --optimise --standalone main "
        <> "--to " <> out <> " --build-path " <> (buildDir </> "pulp-output")

    buildDir </> "main.js" %> \out -> do
      sources <- getDirectoryFiles "" ["src//*.purs", "src//*.js", "psc-package.json"]
      need sources
      liftIO $ callCommand $
        "pulp browserify --standalone main "
        <> "--to " <> out <> " --build-path " <> (buildDir </> "pulp-output")

readLocalConfig :: IO LocalConfig
readLocalConfig = do
    content <- BS.readFile "shake/LocalConfig.json"
    let eitherConfig = Aeson.eitherDecode' (LBS.fromStrict content)
    case eitherConfig of
      Left errMsg  -> error errMsg
      Right config -> pure config
