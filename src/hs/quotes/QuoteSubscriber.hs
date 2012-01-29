{-# OPTIONS_GHC -XDeriveDataTypeable #-} 

import Control.Monad
import System.IO
import System.Exit
import System.Environment
import Control.Exception
import Data.Ratio
import Data.Typeable
import Data.Data
import Data.Time (UTCTime, getCurrentTime)

import qualified System.ZMQ3 as ZMQ
import qualified Data.ByteString as SB
import qualified Data.ByteString.UTF8 as SB


data Quote = Quote {
  symbol :: String,
  date :: UTCTime,
  ask :: Rational,
  bid :: Rational,
  bidVolume :: Rational,
  askVolume :: Rational
} deriving (Eq, Show, Data, Typeable)


main :: IO ()
main = do
    args <- getArgs
    when (length args < 1) $ do
        hPutStrLn stderr "usage: display <address> [<address>, ...]"
        exitFailure

    let x  = SB.fromString "{\"bid_volume\": 24.8, \"symbol\": \"EURUSD\", \"ask_volume\": 50.3, \"ask\": 1.3368, \"date\": \"2007-04-02 12:38:17\", \"bid\": 1.3367}"

    

    ZMQ.withContext 1 $ \c ->
        ZMQ.withSocket c ZMQ.Sub $ \s -> do
            ZMQ.subscribe s ""
            mapM (ZMQ.connect s) args
            forever $ do
                line <- ZMQ.receive s
                SB.putStrLn line
                hFlush stdout
                
