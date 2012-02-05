import Control.Monad

import System.Exit
import System.Environment
import System.IO

import qualified System.ZMQ3 as ZMQ

import FourierFx.Storage (redisConnect, processQuoteString)



-- 
-- The main entry point for the quote subscriber
-- which listens to quotes and initiates computations
-- on quotes.
--
quoteSubscriberMain :: IO ()
quoteSubscriberMain = do
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
                redisConnect >>= processQuoteString line


-- Main entry
main :: IO ()
main = quoteSubscriberMain

