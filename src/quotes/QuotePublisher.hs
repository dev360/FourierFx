import Control.Applicative
import Control.Monad
import System.IO
import System.IO.Error
import System.Exit
import System.Environment
import qualified System.ZMQ3 as ZMQ
import qualified Data.ByteString.UTF8 as SB
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import Codec.Binary.UTF8.String(encode)



-- | Sends a message on a socket, with a given topic.
sendMessage :: ZMQ.Sender a => ZMQ.Socket a -> SB.ByteString -> SB.ByteString -> IO()
sendMessage socket topic message = do
    ZMQ.send socket [] (SB.append topic message)
    when (message /= SB.fromString "") $ return ()


-- | main program loop
main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
        hPutStrLn stderr "usage: prompt <address>"
        exitFailure
    let addr = args !! 0
        name = SB.append (SB.fromString "test") (SB.fromString ": ")
    
    ZMQ.withContext 1 $ \c ->
        ZMQ.withSocket c ZMQ.Pub $ \s -> do
            ZMQ.bind s addr
            forever $ do
                line <- (SB.fromString <$> getLine)
                sendMessage s name line
                --ZMQ.send s [] (SB.append name line)



{-|

                line <- SB.fromString <$> getLine `catch`
                    \e -> if isEOFError e then return "" 
                        else ioError e

                if line /= (SB.fromString "") then
                    ZMQ.send s [] (SB.append name line)
                else
                    return ()                    

|-}
