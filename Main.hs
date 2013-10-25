{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable #-}

import Control.Applicative
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import qualified Data.Set as S
import System.Console.CmdArgs.Implicit
defaultUrl = "http://vpustotu.ru/moderation/"

type Quote = String
data Config = Config { url           :: String
                     , threads       :: Int
                     , maxDuplicates :: Int
                     , dumpPeriod    :: Int
                     } deriving (Show, Data, Typeable)

data ProgramState = Running | Stopping

data Runtime = Runtime { quotes     :: TVar (S.Set Quote)
                       , duplicates :: TVar (Int)
                       , state      :: TVar (ProgramState)
                       }

config = Config { url = defaultUrl &= help "URL to grab from"
                , threads = 5 &= help "Max threads"
                , maxDuplicates = 200 &= explicit &= name "duplicates" &= name "d"
                                  &= help "Duplicates threshold to stop"
                , dumpPeriod = 2 &= explicit &= name "showEvery" &= name "s"
                               &= help "Show progress every N seconds"
                } &= summary "Quotes Grabber"

main = print =<< cmdArgs config

prepareRuntime = Runtime <$> newTVar S.empty <*> newTVar 0 <*> newTVar Running

atomicallyR = lift . atomically

startWorkers :: IO () -> ReaderT Int IO ()
startWorkers job = do
  count <- ask
  counter <- atomicallyR $ newTVar count
  replicateM_ count $ run counter
  atomicallyR $ waitForZero counter
  where
    run c = lift $ forkIO $ job `finally` (atomReplace c (\x -> x - 1))
    atomReplace c = atomically . modifyTVar c
    waitForZero c = readTVar c >>= \x ->
      if x == 0 then return () else retry

startWorkersC :: IO () -> ReaderT Config IO ()
startWorkersC = (withReaderT threads) . startWorkers
