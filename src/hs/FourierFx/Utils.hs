-- |
-- Module:      FourierFx.Utils
-- Copyright:   (c) 2012 Christian Toivola
-- 
-- Utilities
--

module FourierFx.Utils
    (
        dateToString
      , stringToDate
      , DateParts
      , getDateParts    
    ) where

import Data.Time
import Data.Maybe
import System.Locale (defaultTimeLocale)


-- Converts a string to a UTC Time
-- 
-- This is for the default python datetime representation
--
stringToDate :: [Char] -> Maybe UTCTime
stringToDate text = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" text :: Maybe UTCTime


-- Converts a UTCTime to a string
--
-- This is for the default python datetime representation
--
dateToString :: Maybe UTCTime -> [Char]
dateToString date
    | isJust date = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ fromJust date
    | isNothing date = ""


-- Date part representation
data DateParts = DateParts {
  year    :: Int,
  month   :: Int,
  day     :: Int,
  hour    :: Int,
  minute  :: Int,
  second  :: Int
} deriving (Show)


-- Gets the date parts so we can parse out 
-- Year, Month, Day, Hour, Minute and Second
getDateParts :: Maybe UTCTime -> DateParts
getDateParts date
    | isNothing date = DateParts 0 0 0 0 0 0
    | isJust date = DateParts year month day hour minute second
        where
            year = read (formatTime defaultTimeLocale "%Y" $ fromJust date) :: Int
            month = read (formatTime defaultTimeLocale "%m" $ fromJust date) :: Int
            day = read (formatTime defaultTimeLocale "%d" $ fromJust date) :: Int
            hour = read (formatTime defaultTimeLocale "%H" $ fromJust date) :: Int
            minute = read (formatTime defaultTimeLocale "%M" $ fromJust date) :: Int
            second = read (formatTime defaultTimeLocale "%S" $ fromJust date) :: Int
