module OSXIdentHandler (handleQuery) where

import UNIXIdentHandler

handleQuery :: Int -> Int -> IO (String, String) -- do the lookup
handleQuery = unixHandler syse

syse :: UnixCmd
syse lport fport =
  "lsof -nP -iTCP | " ++  --list all open TCP connections numerically
  "awk '$9~/^.+:" ++ show lport ++ ".+:" ++ show fport ++ "$/ " ++  --if the address matches
  "{ print $3; }; " ++                                              --print the username
  "END { print \"\\n\";}'" --always print a newline, so hGetLine never fails
