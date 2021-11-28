module Error (Error (..), Result, ResultT, justOrThrow, liftResult, expect, handleIOException, handleHTTPException) where

import Control.Exception (IOException)
import Control.Monad.Catch (MonadCatch, handle)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (Except, ExceptT, mapExceptT, runExcept, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), maybeToExceptT)
import Data.Functor.Identity (Identity (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Network.HTTP.Client.Conduit (HttpException (HttpExceptionRequest, InvalidUrlException), HttpExceptionContent (StatusCodeException))
import System.Exit (die)

newtype Error = Error String deriving (Show)

type Result a = Except Error a

type ResultT m a = ExceptT Error m a

justOrThrow :: Error -> Maybe a -> Result a
justOrThrow = (. (MaybeT . Identity)) . maybeToExceptT

liftResult :: (Monad m) => Result a -> ResultT m a
liftResult = mapExceptT $ return . runIdentity

-- |
-- 'expect' @message@ @result@ - Unwraps the given results, or kills the program on error.
expect :: String -> ResultT IO a -> IO a
expect msg r = do
  result <- runExceptT r
  case result of
    Left (Error e) -> die $ msg ++ ": " ++ e
    Right a -> return a

handleIOException :: (MonadCatch m) => ResultT m a -> ResultT m a
handleIOException = mapExceptT (handle handler)
  where
    handler :: (Monad m) => IOException -> m (Either Error a)
    handler = return . Left . Error . show

handleHTTPException :: (MonadIO m, MonadCatch m) => ResultT m a -> ResultT m a
handleHTTPException = mapExceptT $ handle (return . Left . Error . handler)
  where
    handler (InvalidUrlException url reason) = "Invalid url " ++ url ++ ": " ++ reason
    handler (HttpExceptionRequest _ content) = case content of
      StatusCodeException _ reason -> T.unpack $ E.decodeUtf8 reason
      e -> show e
