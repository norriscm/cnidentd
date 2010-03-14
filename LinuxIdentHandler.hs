module LinuxIdentHandler (handleQuery) where

import UNIXIdentHandler

import System.Process (runInteractiveCommand)
import System.IO

handleQuery :: Int -> Int -> IO (String, String) -- do the lookup
handleQuery = unixHandler syse

syse :: Int -> Int -> String
syse lport fport =
  "lsof -nP -iTCP | " ++  --list all open TCP connections numerically
  "awk '$8~/^.+:" ++ (show lport) ++ ".+:" ++ (show fport) ++ "$/ " ++  --if the address matches
  "{ print $3; }; " ++                                              --print the username
  "END { print \"\n\";}'" --always print a newline, so hGetLine never fails

