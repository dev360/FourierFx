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
      , DateParts(..)
      , TimeSeriesMap(..)
      , getDateParts
      , getTimeSeriesMap
      , nextYear
      , nextMonth
      , nextDay
      , nextHour
      , nextMinute

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
  year          :: Int,
  month         :: Int,
  day           :: Int,
  hour          :: Int,
  minute        :: Int,
  second        :: Int,
  fullDate      :: Maybe UTCTime
} deriving (Show)


--
-- Bitmap to determine which part of a 
-- date has entered a new time series.
data TimeSeriesMap = TimeSeriesMap {
  newYear       :: Bool,
  newMonth      :: Bool,
  newDay        :: Bool,
  newHour       :: Bool,
  newMinute     :: Bool
} deriving (Show)


-- Gets the start of next year from the
-- current date
nextYear :: Maybe UTCTime -> Maybe UTCTime
nextYear date
    | isNothing date = Nothing
    | isJust date = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ( yearS' ++ "-01-01 00:00:00" ) :: Maybe UTCTime
        where
            dateParts = getDateParts $ date
            year' = year dateParts + 1
            yearS' = show year'


-- Gets the start of next month from
-- the current date
nextMonth :: Maybe UTCTime -> Maybe UTCTime
nextMonth date
    | isNothing date = Nothing
    | isJust date = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ( yearS' ++ "-" ++ monthS' ++ "-01 00:00:00" ) :: Maybe UTCTime
        where
            dateParts = getDateParts date
            yearS' = case (month dateParts) of
                        12  -> show ((year dateParts) + 1)
                        _   -> show (year dateParts)
            
            -- Account for idiotic trailing zeros.
            month' = case (month dateParts) of
                        12  -> 1
                        _   -> (month dateParts) + 1

            monthS' = case (month' < 10) of
                        True -> "0" ++ show month'
                        False -> show month'


-- Gets the start of the next day from
-- the current date
nextDay :: Maybe UTCTime -> Maybe UTCTime
nextDay date
    | isNothing date = Nothing
    | isJust date = Just $ UTCTime dayPart' timeDiff'
        where
            dayPart = utctDay $ fromJust date
            (year', month', day') = toGregorian dayPart

            yearS' = show year'

            -- Account for idiotic trailing zeros.
            monthS' = case (month' < 10) of
                        True -> "0" ++ show month'
                        False -> show month'

            dayS' = case (day' < 10) of
                        True -> "0" ++ show day'
                        False -> show day'

            Just dateWithoutTime = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (yearS' ++ "-" ++ monthS' ++ "-" ++ dayS' ++ " 00:00:00") :: Maybe UTCTime
            dayPart' = addDays 1 dayPart
            timeDiff' = utctDayTime dateWithoutTime


-- Gets the start of the next hour from the
-- current date.
nextHour :: Maybe UTCTime -> Maybe UTCTime
nextHour date
    | isNothing date = Nothing
    | isJust date = date'
        where
            hour = read (formatTime defaultTimeLocale "%H" $ fromJust date) :: Int
            hour' = case hour of
                        23  -> 0
                        _   -> hour + 1
            
            dayPart = utctDay $ fromJust date
            dayPart' = case hour of
                        23  -> addDays 1 dayPart
                        _   -> dayPart
            
            (year', month', day') = toGregorian dayPart  
            yearS' = show year'
            -- Account for idiotic trailing zeros.
            monthS' = case (month' < 10) of
                        True -> "0" ++ show month'
                        False -> show month'

            dayS' = case (day' < 10) of
                        True -> "0" ++ show day'
                        False -> show day'

            hourS' = case (hour' < 10) of
                        True -> "0" ++ show hour'
                        False -> show hour'

            date' = parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (yearS' ++ "-" ++ monthS' ++ "-" ++ dayS' ++" "++ hourS' ++ ":00:00") :: Maybe UTCTime


nextMinute :: Maybe UTCTime -> Maybe UTCTime
nextMinute date
    | isNothing date = Nothing
    | isJust date = date'
        where
            (year', month', day') = toGregorian $ utctDay $ fromJust date

            hour' = read (formatTime defaultTimeLocale "%H" $ fromJust date) :: Int
            minute = read (formatTime defaultTimeLocale "%M" $ fromJust date) :: Int
            minute' = if minute == 59 then 0 else minute + 1  -- The 59 case is actually handled properly below but oh well.

            yearS' = show year'
            -- Account for idiotic trailing zeros.
            monthS' = case (month' < 10) of
                        True    -> "0" ++ show month'
                        False   -> show month'

            dayS' = case (day' < 10) of
                        True    -> "0" ++ show day'
                        False   -> show day'

            hourS' = case (hour' < 10) of
                        True    -> "0" ++ show hour'
                        False   -> show hour'

            minuteS' = case (minute' < 10) of
                        True    -> "0" ++ show minute'
                        False   -> show minute'

            date' = case minute of
                      59  -> nextHour date
                      _   -> parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (yearS' ++ "-" ++ monthS' ++ "-" ++ dayS' ++" "++ hourS' ++ ":"++ minuteS' ++":00") :: Maybe UTCTime
            



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
        

-- Gets the TimeSeriesMap between two dates.
--   What this allows you to do is, you can determine
--   if the new, incoming quote is part of the next
--   interval window (year, month, day, hour, minute).
--
getTimeSeriesMap :: Maybe UTCTime -> Maybe UTCTime -> TimeSeriesMap
getTimeSeriesMap last latest
    | isNothing last || isNothing latest = TimeSeriesMap False False False False False
    | isJust last && isJust latest = TimeSeriesMap isNewYear isNewMonth isNewDay isNewHour isNewMinute
        where
            isNewYear =  fromJust latest >= fromJust (nextYear last)
            isNewMonth = fromJust latest >= fromJust (nextMonth last) 
            isNewDay = fromJust latest >= fromJust (nextDay last)
            isNewHour = fromJust latest >= fromJust (nextHour last)
            isNewMinute = fromJust latest >= fromJust (nextMinute last)

