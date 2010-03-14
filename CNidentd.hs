module Main (main) where

--a platform-specific module does the heavy lifting to answer queries
--import SimpleIdentHandler
import OSXIdentHandler

import Network
import System.IO
import System.Timeout
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Control.Concurrent (forkIO)
import Control.Exception (finally)
--import System.Posix.Daemonize (serviced)
import Char

main :: IO ()
main = withSocketsDo $ do
  s <- listenOn $ PortNumber 113
  forever $ do
    (h, host, port) <- accept s
    forkIO (handleConnection h)

handleConnection :: Handle -> IO ()  -- this is the control flow of each connection thread
handleConnection h =  finally hndlr cleanup where
  hndlr = withTimeout $ do
    hSetBuffering h LineBuffering
    query <- hGetLine h
    let (lport, fport) = parseQuery query
    (resptype, addinfo) <- handleQuery lport fport
    let resp = concat [show lport, ", ", show fport, " : ", resptype, " : ", addinfo, "\r\n"]
    hPutStr h resp
  cleanup = do
    hClose h

withTimeout :: IO () -> IO () -- 30 second timeout per RFC
withTimeout a = do
  r <- timeout (30*(10^6)) a
  return (fromMaybe () r)

parseQuery :: String -> (Int, Int)
parseQuery q = (read lport, read fport) where
  (lport, rst) = span isDigit $ dropWhile isSpace q
  (fport, _) = span isDigit $ dropWhile (not . isDigit) rst