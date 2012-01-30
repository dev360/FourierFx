{-# OPTIONS_GHC -XDeriveDataTypeable #-} 
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad
import System.IO
import System.Exit
import System.Environment
import Control.Exception
import Control.Monad (forM_)

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

import Database.Redis.Redis (Redis, Reply, connect, lpush, set, llen, lrange, Reply (RBulk))
import Database.Redis.ByteStringClass (BS, toBS, fromBS)

-- Quote datastructure
data Quote = Quote {
  symbol :: String,
  date :: String,
  ask :: Rational,
  bid :: Rational,
  askVolume :: Rational,
  bidVolume :: Rational
} deriving (Eq, Show, Data, Typeable)


-- Converts a quote into json
instance ToJSON Quote where
    toJSON (Quote symbol date ask bid askVolume bidVolume) = object [ "symbol"      .= symbol,
                                                  "date"        .= date,
                                                  "ask"         .= ask,
                                                  "bid"         .= bid,
                                                  "ask_volume"  .= askVolume,
                                                  "bid_volume"  .= bidVolume ]

-- Converts json into a quote
instance FromJSON Quote where
    parseJSON (Object v) = Quote <$>
                         v .: "symbol" <*>
                         v .: "date" <*>
                         v .: "ask" <*>
                         v .: "bid" <*>
                         v .: "ask_volume" <*>
                         v .: "bid_volume"
    parseJSON _          = empty


getQuoteHistory :: Redis -> [Char] -> IO (Maybe (Maybe SB.ByteString))
getQuoteHistory redis key = do
    reply <- lrange redis (0 1001)
    return $ case reply of 
                  RBulk (Just r) -> Just $ decode r
                  _ -> Nothing
    

processQuote :: SB.ByteString -> Redis -> IO ()
processQuote line redis = do
    let json = LB.fromChunks [line]
    let quote = decode json :: Maybe Quote
    case quote of 
        Nothing -> return ()
        Just quote -> do
            let key = "symbol_" ++ (symbol quote)
            lpush redis key line
            --let history = getQuoteHistory redis key
            history <- getQuoteHistory redis key
            return ()


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
                connect "localhost" "6379" >>= processQuote line



{--

saveQuote :: Redis -> Maybe Quote -> IO ()
saveQuote redis Nothing = return ()
saveQuote redis (Just quote) = do
    let key = "symbols_" ++ (symbol quote)
    print key
    let json = encode json
    lpush redis key json
    set redis key $ SB.fromString "test1"
    return ()


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

                -- Get json from line
                let json = LB.fromChunks [line]
                let quote = decode json :: Maybe Quote

                conn <- connect "localhost" "6379"
                saveQuote conn quote
                print "test"
                hFlush stdout
                


--}
