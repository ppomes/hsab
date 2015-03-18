module Main (main) where

import Control.Monad
import Data.Foldable (foldlM)
import Data.Maybe
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import Control.Concurrent 
import Network.HTTP
import Network.URI
import System.Posix.Unistd

data Options = Options {
  optNumber :: Int
  , optConcurrency :: Int
  , optURL :: String
  } deriving Show

defaultOptions = Options {
  optNumber = 1
  , optConcurrency = 1
  , optURL = "http://localhost"
  }

options :: String -> [OptDescr (Options -> IO Options)]
options helpMessage =
  [ Option ['n'] ["numbers"]
    (ReqArg (\str opts -> do
                let n = read str
                return $ opts { optNumber = n }) "num")
    "number of request per thread"
  , Option ['c'] ["concurrency"]
    (ReqArg (\str opts -> do
                let c = read str
                return $ opts { optConcurrency = c }) "num")
    "number of threads"
  , Option ['u'] ["URL"]
    (ReqArg (\str opts -> do
	       let u = str
               return $ opts { optURL = u }) "str")
    "URL"
  ]

getUrl :: Options -> String
getUrl ( Options { optURL = u }  ) = u

getConcurrency :: Options -> Int
getConcurrency ( Options { optConcurrency = c }  ) = c

getNumber :: Options -> Int
getNumber ( Options { optNumber = n } ) = n

parseArgs :: IO Options
parseArgs = do
  argv <- getArgs
  progName <- getProgName
  let header = "Usage: " ++ progName ++ " [OPTION...]"
  let helpMessage = usageInfo header (options "")
  case getOpt RequireOrder (options helpMessage) argv of
    (opts, [], []) -> foldlM (flip id) defaultOptions opts
    (_, _, errs) -> ioError (userError (concat errs ++ helpMessage))
 
downloadURL :: String -> IO ()
downloadURL url =
   do resp <- simpleHTTP request
      return (); 
   where request = Request {rqURI = uri,
		            rqMethod = GET,
			    rqHeaders = [],
			    rqBody = ""}
	 uri = fromJust $ parseURI url

worker :: Int -> String -> IO ()
worker n url = 
   do tId <- myThreadId
      putStrLn (show tId)
      replicateM_ n $ downloadURL url


myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
   mvar <- newEmptyMVar
   forkFinally io (\_ -> putMVar mvar ())
   return mvar

waitvar :: MVar () -> IO ()
waitvar mvar = readMVar mvar

launch :: Options -> IO ()
launch options = 
   do res <- replicateM c (myForkIO $ worker n url) -- fork requested number of threads
      mapM waitvar res -- wait for threads 
      return ();
   where c = getConcurrency options
         n = getNumber options
         url = getUrl options

main :: IO ()
main = do
  options <- parseArgs
  launch options
