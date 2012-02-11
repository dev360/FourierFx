-- |
-- Module:      FourierFx
-- Copyright:   (c) 2012 Christian Toivola
-- 
-- FourierFx library for detecting patterns in 
-- stock and commodity prices.
--

module FourierFx
    (
          Quote
        , parseQuote 
        , getSymbolQuotes
        , processQuoteString
        , redisConnection

        , dateToString
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


import FourierFx.Storage (  
          Quote
        , parseQuote 
        , getSymbolQuotes
        , processQuoteString
        , redisConnect
    )


import FourierFx.Utils (    
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
    )
