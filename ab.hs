module Main (main) where

import Control.Monad
import Control.Concurrent
import Network.HTTP
import Network.URI
import Control.Exception
import Options.Applicative
import Data.Either
import Data.List

newtype RequestCount = RequestCount Int
newtype ThreadCount  = ThreadCount Int
newtype URL          = URL String

data Options = Options RequestCount ThreadCount URL

parseOps :: Parser Options
parseOps = Options <$> requestCount <*> concurrency <*> url
    where
        requestCount = RequestCount <$> option auto (  long "numbers"
                                                    <> short 'n'
                                                    <> help "Number of requests per thread"
                                                    <> value 1)
        concurrency = ThreadCount <$> option auto (  long "concurrency"
                                                  <> short 'c'
                                                  <> help "Number of threads"
                                                  <> value 1)
        url = URL <$> strOption (  long "URL"
                                <> short 'u'
                                <> help "Queried URL"
                                <> value "http://localhost")

downloadURL :: URL -> IO ()
downloadURL (URL url) = case parseURI url of
                            Nothing -> error ("Could not parse this url: " <> url)
                            Just rq -> void $ simpleHTTP Request { rqURI     = rq
                                                                 , rqMethod  = GET
                                                                 , rqHeaders = []
                                                                 , rqBody    = ""}

worker :: RequestCount -> URL -> IO ( String )
worker (RequestCount n) url = do
  replicateM_ n $ downloadURL url
  iId <- myThreadId
  return ((show iId) ++ " completed " ++ (show n) ++ " request(s)")

myForkIO :: IO a -> IO (MVar (Either SomeException a))
myForkIO io = do
   mvar <- newEmptyMVar
   void $ forkFinally io (putMVar mvar)
   return mvar

launch :: Options -> IO ()
launch (Options n (ThreadCount c) url) =
   do res <- replicateM c (myForkIO $ worker n url)      -- fork requested number of threads
      results <- mapM readMVar res                       -- wait for threads
      mapM_ putStrLn $ nub ( map show $ lefts results )  -- display unique exceptions
      mapM_ putStrLn $ rights results                    -- display threads results

main :: IO ()
main = execParser opts >>= launch
    where
        opts = info (helper <*> parseOps) (fullDesc <> progDesc "An ab-like utility")
