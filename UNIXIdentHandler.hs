module UNIXIdentHandler (unixHandler, UnixCmd) where

import System.Process (runInteractiveCommand)
import System.IO

type UnixCmd = Int -> Int -> String

unixHandler :: UnixCmd -> Int -> Int -> IO (String, String) -- do the lookup
unixHandler syse lport fport = do
  putStrLn (syse lport fport)
  (_,out,_,proc) <- runInteractiveCommand (syse lport fport)
  hSetBuffering out LineBuffering
  name <- hGetLine out
  hClose out
  if length name > 0
    then return ("USERID", "UNIX : " ++ name)
    else return ("ERROR","UNKNOWN-ERROR")
