{-# OPTIONS_GHC -XDeriveDataTypeable #-} 
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad
import System.IO
import System.Exit
import System.Environment
import Control.Exception

import Data.Aeson
import Data.Data
import Data.Ratio
import Data.Time (UTCTime, getCurrentTime)
import Data.Typeable

import Text.JSON.Types

import qualified System.ZMQ3 as ZMQ
import qualified Data.ByteString as SB
import qualified Data.ByteString.UTF8 as SB
import qualified Data.ByteString.Lazy as LB

data Quote = Quote {
  symbol :: String,
  date :: String,
  ask :: Rational,
  bid :: Rational,
  askVolume :: Rational,
  bidVolume :: Rational
} deriving (Eq, Show, Data, Typeable)


instance ToJSON Quote where
    toJSON (Quote symbol date ask bid askVolume bidVolume) = object [ "symbol"      .= symbol,
                                                  "date"        .= date,
                                                  "ask"         .= ask,
                                                  "bid"         .= bid,
                                                  "ask_volume"  .= askVolume,
                                                  "bid_volume"  .= bidVolume ]



instance FromJSON Quote where
    parseJSON (Object v) = Quote <$>
                         v .: "symbol" <*>
                         v .: "date" <*>
                         v .: "ask" <*>
                         v .: "bid" <*>
                         v .: "ask_volume" <*>
                         v .: "bid_volume"
    parseJSON _          = empty



--{"bid_volume": 17.5, "symbol": "EURUSD", "ask_volume": 0.5, "ask": 1.3367, "date": "2007-04-02 11:56:31", "bid": 1.3366}



main1 :: IO ()
main1 = do
    --let req = decode SB.fromString "{\"bid_volume\": 24.8, \"symbol\": \"EURUSD\", \"ask_volume\": 50.3, \"ask\": 1.3368, \"date\": \"2007-04-02 12:38:17\", \"bid\": 1.3367}" :: Maybe Quote
    print "test"


main :: IO ()
main = do
    args <- getArgs
    when (length args < 1) $ do
        hPutStrLn stderr "usage: display <address> [<address>, ...]"
        exitFailure

    ZMQ.withContext 1 $ \c ->
        ZMQ.withSocket c ZMQ.Sub $ \s -> do
            ZMQ.subscribe s ""
            mapM (ZMQ.connect s) args
            forever $ do
                line <- ZMQ.receive s

                let json = LB.fromChunks [line]
                let quote = decode json :: Maybe Quote
                print quote

                hFlush stdout
                
