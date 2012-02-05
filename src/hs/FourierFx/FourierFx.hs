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
        , redisConnect
    ) where


import FourierFx.Storage (    Quote
                            , parseQuote 
                            , getSymbolQuotes
                            , processQuoteString
                            , redisConnect)
