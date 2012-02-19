{-# OPTIONS_GHC -XDeriveDataTypeable #-} 
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      FourierFx.Storage
-- Copyright:   (c) 2012 Christian Toivola
-- 
-- Storage components
--

module FourierFx.Storage
    (
        Quote(..)
      , parseQuote 
      , getSymbolQuotes
      , processQuoteString
      , redisConnect
    ) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Exception
import Control.Monad

import qualified Data.ByteString as SB
import qualified Data.ByteString.UTF8 as SB
import qualified Data.ByteString.Lazy as LB
import Data.Data
import Data.Maybe
import Data.Ratio
import Data.Time
import Data.Typeable

import System.IO
import System.Locale (iso8601DateFormat, defaultTimeLocale)

import Data.Aeson
import Database.Redis.Redis (Redis, Reply, connect, lpush, set, llen, lrange, Reply (RBulk), fromRMultiBulk)
import Database.Redis.ByteStringClass (BS, toBS, fromBS)
import Text.JSON.Types

import FourierFx.Utils (TimeSeriesMap, getTimeSeriesMap)


-- Quote datastructure
data Quote = Quote {
  symbol :: String,
  date :: Maybe UTCTime,
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

--
-- Parses a quote from a json byte string
--
parseQuote :: Maybe LB.ByteString -> Maybe Quote
parseQuote json
    | isJust json = decode (fromJust json) :: Maybe Quote
    | isNothing json = Nothing


--
-- Retrieves the quote history for a symbol in a desired interval (start, end)
--
getSymbolQuotes :: Redis -> [Char] -> (Int, Int) -> IO [Maybe Quote] 
getSymbolQuotes redis symbol range = do
    -- Get a thousand quotes for the symbol in question.
    jsonQuotes <- lrange redis symbol range >>= fromRMultiBulk :: IO (Maybe [Maybe LB.ByteString])
    case jsonQuotes of
          Just jsonQuotes -> return $ map parseQuote jsonQuotes
          Nothing -> return []


--
-- Takes a json string, converts it to a quote, then saves it
-- and schedules any relevant computations that need to take place.
--
processQuoteString :: SB.ByteString -> Redis -> IO ()
processQuoteString line redis = do
    let json = LB.fromChunks [line]
    let quote = decode json :: Maybe Quote
    case quote of 
        Nothing -> return ()
        Just quote -> do
            -- First, push the quote into redis as a simple
            -- json string (for portability)
            let quoteSymbol = "symbol_" ++ (symbol quote)
            lpush redis quoteSymbol line

            -- Now retrieve the quote history for the symbol
            --  .. we only need 10 items to work with.
            --
            history <- getSymbolQuotes redis quoteSymbol (0, 10)

            let latest  = history !! 0
            let last    = history !! 1

            -- Now get the time series map to determine what to do next.
            let timeSeries = getTimeSeriesMap (date $ fromJust last) (date $ fromJust latest)
            

            return ()


-- 
-- Returns a redis connection to use
--
redisConnect :: IO Redis
redisConnect = connect "localhost" "6379"

