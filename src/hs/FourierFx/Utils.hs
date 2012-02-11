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
      , IntervalBitMap
      , getDateParts
      , isNewInterval
      , getIntervalBitMap
      , nextYear
      , nextMonth
      , nextDay

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
  second  :: Int,
  fullDate :: Maybe UTCTime
} deriving (Show)


--
-- Bitmap to determine which part of a 
-- date has entered a new time interval.
data IntervalBitMap = IntervalBitMap {
  newYear   :: Bool,
  newMonth  :: Bool,
  newDay    :: Bool,
  newHour   :: Bool,
  newMinute   :: Bool
} deriving (Show)


-- Gets the start of next year from the
-- current date
nextYear :: UTCTime -> UTCTime
nextYear date = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ( show year ++ "-1-1 00:00:00" )
    where
          dateParts = getDateParts $ Just date
          year month = year dateParts + 1


-- Gets the start of next month from
-- the current date
nextMonth :: UTCTime -> UTCTime
nextMonth date = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ( show year ++ "-" ++ show month ++ "-1 00:00:00" )
    where
          dateParts = getDateParts $ Just date
          year month = case month dateParts of
                    12 -> return (year dateParts + 1) (1)
                    _ -> return (year dateParts) (month dateParts + 1)


-- Gets the start of the next day from
-- the current date
nextDay :: UTCTime -> UTCTime
nextDay date = UTCTime day' timeDiff'
    where
        -- Discard time portion to make sure its 00:00:00
        year = read (formatTime defaultTimeLocale "%Y" $ fromJust date) :: Int
        month = read (formatTime defaultTimeLocale "%m" $ fromJust date) :: Int
        day = read (formatTime defaultTimeLocale "%d" $ fromJust date) :: Int
        Just dateOnly = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (show year ++ "-" ++ show month ++ "-" ++ show day ++" 00:00:00") :: Maybe UTCTime
        
        day' = addDays 1 (utctDay date)
        timeDiff' = utctDayTime dateOnly


-- Gets the date parts so we can parse out 
-- Year, Month, Day, Hour, Minute and Second
getDateParts :: Maybe UTCTime -> DateParts
getDateParts date
    | isNothing date = DateParts 0 0 0 0 0 0 Nothing
    | isJust date = DateParts year month day hour minute second fullDate
        where
            year = read (formatTime defaultTimeLocale "%Y" $ fromJust date) :: Int
            month = read (formatTime defaultTimeLocale "%m" $ fromJust date) :: Int
            day = read (formatTime defaultTimeLocale "%d" $ fromJust date) :: Int
            hour = read (formatTime defaultTimeLocale "%H" $ fromJust date) :: Int
            minute = read (formatTime defaultTimeLocale "%M" $ fromJust date) :: Int
            second = read (formatTime defaultTimeLocale "%S" $ fromJust date) :: Int
            fullDate = date


-- I wanted to curry this and pass it to a map over the interval strings,
-- but that idea didnt work out :(
isNewInterval :: DateParts -> DateParts -> [Char] -> Bool
isNewInterval prev new interval = True
            



            

-- Gets the IntervalBitMap between two dates.
-- 
getIntervalBitMap :: DateParts -> DateParts -> IntervalBitMap
getIntervalBitMap prev new = 
    IntervalBitMap isNewYear isNewMonth isNewDay isNewHour isNewMinute
        where
            isNewYear = isNewInterval prev new "year"
            isNewMonth = isNewInterval prev new "month"
            isNewDay = isNewInterval prev new "day"
            isNewHour = isNewInterval prev new "hour"
            isNewMinute = isNewInterval prev new "minute"
