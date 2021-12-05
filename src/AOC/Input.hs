module AOC.Input (fetchInput) where

import AOC.Config (AOCConfig (day, token, year), aocDirectory)
import AOC.Error (Error (..), Result, ResultT, handleHTTPException, handleIOException, justOrThrow, liftResult)
import Control.Monad (liftM2)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (groupBy)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Network.HTTP.Simple
  ( addRequestHeader,
    getResponseBody,
    httpBS,
    parseRequestThrow,
  )
import Network.HTTP.Types.Header (HeaderName, hCookie)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Text.Printf (printf)

headerCookie :: AOCConfig -> Result (HeaderName, ByteString)
headerCookie cfg = do
  t <- justOrThrow (Error "Missing authentification token") $ token cfg
  return (hCookie, (E.encodeUtf8 . T.pack) ("session=" ++ t))

aocURL :: AOCConfig -> String
aocURL = liftM2 (printf "https://adventofcode.com/%u/day/%u/input") year day

downloadInput :: (MonadIO m, MonadCatch m) => AOCConfig -> ResultT m Text
downloadInput cfg = handleHTTPException $
  do
    let url = aocURL cfg
    liftIO $ printf "Downloading input from '%s'...\n" url
    -- create the initial request
    initialReq <- parseRequestThrow url
    -- create the authentication cookie
    cookie <- liftResult $ headerCookie cfg
    -- add the cookie to the request
    let req = uncurry addRequestHeader cookie initialReq
    -- sent the request and decode the response
    E.decodeUtf8 . getResponseBody <$> httpBS req

inputCachePath :: AOCConfig -> FilePath
inputCachePath = liftM2 (printf "./%s/%u/%u.txt" aocDirectory) year day

readInputCache :: AOCConfig -> ResultT IO (Maybe Text)
readInputCache cfg = do
  let path = inputCachePath cfg
  exists <- lift $ doesFileExist path
  if not exists
    then return Nothing
    else do
      lift $ printf "Fetching input from cache file %s...\n" path
      input <- handleIOException $ lift $ BS.readFile path
      return $ Just $ E.decodeUtf8 input

writeInputCache :: AOCConfig -> Text -> ResultT IO ()
writeInputCache cfg input = handleIOException $
  lift $ do
    let path = inputCachePath cfg
    let dirs = concat $ init $ groupBy (\_ b -> b /= '/') path
    createDirectoryIfMissing True dirs
    BS.writeFile (inputCachePath cfg) (E.encodeUtf8 input)

-- |
-- 'fetchInput' @config@ - Attempts to fetch the desired input.
--
-- Input is read from cache if available, or downloaded from the AOC website using the auth token in the config.
fetchInput :: AOCConfig -> ResultT IO Text
fetchInput cfg = do
  -- try to read previous input from cache
  cachedInput <- readInputCache cfg
  case cachedInput of
    -- if present, return cached input
    Just ci -> return ci
    -- else try to download it
    Nothing -> do
      input <- downloadInput cfg
      -- write to input cache
      writeInputCache cfg input
      return input
