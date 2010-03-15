module SimpleIdentHandler (handleQuery, handlerInit, HandlerState) where

import System (getArgs, exitFailure)

type HandlerState = String

handleQuery :: String -> Int -> Int -> IO (String, String) -- do the lookup
handleQuery user lport fport = return ("USERID","UNIX : " ++ user)

handlerInit cont = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: sidentd USER" >> exitFailure
    (user:_) -> cont user

