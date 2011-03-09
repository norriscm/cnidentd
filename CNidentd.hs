module Main (main) where

--a platform-specific module does the heavy lifting to answer queries
--import SimpleIdentHandler
import OSXIdentHandler (handleQuery)

import Network
import System.IO
import System.Timeout
import Control.Monad (forever)
import Data.Maybe (fromMaybe, isJust)
import Control.Concurrent (forkIO)
import Control.Exception (finally)
import System.Posix.Daemonize
import Data.Char (isDigit, isSpace)

main :: IO ()
main = serviced simpleDaemon { privilegedAction = bindSocket, program = serve }

bindSocket :: IO Socket
bindSocket = withSocketsDo $ listenOn $ PortNumber 113

serve :: Socket -> IO ()
serve s = forever $ do
  (h, _, _) <- accept s
  forkIO $ handleConnection h

handleConnection :: Handle -> IO ()  -- the control flow of each connection thread
handleConnection h = hndlr `finally` hClose h where
  hndlr = withTimeout $ do
    hSetBuffering h LineBuffering
    query <- hGetLineN h 512
    let (lport, fport) = parseQuery query
    (resptype, addinfo) <- handleQuery lport fport
    let resp = concat [show lport, ", ", show fport, " : ", resptype, " : ", addinfo, "\r\n"]
    hPutStr h resp

withTimeout :: IO () -> IO () -- 30 second timeout per RFC
withTimeout a = fromMaybe () `fmap` timeout (30*(10^6)) a

parseQuery :: String -> (Int, Int)
parseQuery q = (read lport, read fport) where
  (lport, rst) = span isDigit $ dropWhile isSpace q
  (fport, _) = span isDigit $ dropWhile (not . isDigit) rst

hGetLineN :: Handle -> Int -> IO String
hGetLineN h cnt = fmap reverse $ go cnt [] where
  go 0 a = return a
  go n a = do {c <- hGetChar h; if c == '\n' then return a else go (n-1) (c:a)}
