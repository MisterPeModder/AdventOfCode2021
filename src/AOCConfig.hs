{-# LANGUAGE DeriveGeneric #-}

module AOCConfig (AOCConfig (..), aocDirectory, defaultConfig, readConfig, readConfigOrDefault, writeConfig) where

import Control.Exception (IOException, handle)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT (ExceptT), mapExceptT, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), maybeToExceptT)
import Data.Aeson (decodeStrict', encode)
import Data.Aeson.Types (FromJSON, ToJSON (toEncoding), defaultOptions, genericToEncoding)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (fromRight)
import Error (Error (..), Result, ResultT, handleIOException, justOrThrow, liftResult)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import Prelude hiding (readFile, writeFile)

data AOCConfig = AOCConfig
  { year :: Word,
    day :: Word,
    token :: Maybe String
  }
  deriving (Generic, Show)

instance ToJSON AOCConfig where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AOCConfig

aocDirectory :: FilePath
aocDirectory = ".aoc"

configPath :: FilePath
configPath = "./" ++ aocDirectory ++ "/config.json"

defaultConfig :: AOCConfig
defaultConfig = AOCConfig {year = 2021, day = 1, token = Nothing}

readConfig :: ResultT IO AOCConfig
readConfig = handleIOException $ do
  contents <- lift $ BS.readFile configPath
  liftResult $ justOrThrow (Error "Invalid config JSON") $ decodeStrict' contents

readConfigOrDefault :: IO AOCConfig
readConfigOrDefault = fromRight defaultConfig <$> runExceptT readConfig

writeConfig :: AOCConfig -> ResultT IO ()
writeConfig config = handleIOException $
  lift $ do
    createDirectoryIfMissing True aocDirectory
    LBS.writeFile configPath $ encode config
