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
import Text.Printf


-- | Sends a message on a socket, with a given topic.
sendMessage :: ZMQ.Sender a => ZMQ.Socket a -> SB.ByteString -> SB.ByteString -> IO()
sendMessage socket topic message = do
    putStrLn $ SB.toString message
    ZMQ.send socket [] (SB.append topic message)
    when (message /= SB.fromString "") $ return ()


-- | Gets input from stdin
getLine' :: IO String
getLine' = hGetLine stdin `catch`
              \e -> if isEOFError e then return ""
              else ioError e


-- | main program loop
main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
        hPutStrLn stderr "usage: prompt <address>"
        exitFailure
    let addr = args !! 0
        topic = SB.append (SB.fromString "test") (SB.fromString ": ")
    

    when (length args == 2) $ do
        

    ZMQ.withContext 1 $ \c ->
        ZMQ.withSocket c ZMQ.Pub $ \s -> do
            ZMQ.bind s addr
            forever $ do 
                eof <- hIsEOF stdin
                line <- SB.fromString <$> getLine'
                when (eof == False) $ sendMessage s topic line
                --line <- SB.fromString <$> getLine'
                --when (line /= SB.fromString "") $ sendMessage s name line

