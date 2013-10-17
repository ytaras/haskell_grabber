{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Set
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Network.HTTP.Conduit      (simpleHttp)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (intercalate)
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Control.Monad as M
import Control.Applicative hiding (empty)
import Control.Monad.Reader

url = "http://vpustotu.ru/moderation/"
-- TODO full rewrite with channels
main = launchAndWait 10 $ do
  putStrLn "I am thread"
  threadDelay $ 1000 * 1000
  putStrLn "My job is done"


type Quote = String
type Cache a = Set a

data Runtime = Runtime { dump  :: TChan State
                       , quit  :: TChan ()
                       , state :: TVar State
                       }
initRuntime :: STM Runtime
initRuntime =
  Runtime <$> newTChan <*> newTChan <*> newTVar emptyState

start :: Conf -> IO Runtime
start conf = do
  runtime <- atomically initRuntime
  (forkIO . dumpThread . dump) runtime
  launchAndWait (threads conf) $ downloader runtime
  return runtime

dumpThread :: TChan State -> IO ()
dumpThread = undefined

downloader :: Runtime -> IO ()
downloader = undefined

waitFor :: (a -> Bool) -> TVar a -> STM ()
waitFor cond var =
  readTVar var >>= \x -> if cond x then return () else retry

updateTVar :: (a -> a) -> TVar a -> STM a
updateTVar action var = do
  x <- readTVar var
  writeTVar var $ action x
  return x

launchAndWait :: Int -> IO () -> IO ()
launchAndWait amount action = do
  syncVar <- atomically $ newTVar 0
  let
    notifier = atomically $ updateTVar (+1) syncVar
    worker = forkIO $ action `finally` notifier
  forM_ [1..amount] $ \_ -> worker
  atomically $ waitFor (amount==) syncVar

data Conf = Conf { threads  :: !Int
                 , maxDupes :: !Int
                 }
conf = Conf 5 50

data State = State { cache      :: Cache Quote
                   , duplicates :: !Int
                   , counter  :: !Int
                   } deriving Show
emptyState = State empty 0 0
type StateRef = TVar State

type Dump = TChan State
-- TODO We're overusing state word
data GrabberState = Continue | Quit deriving Eq


dumper :: Dump -> IO ()
dumper chan = do
  state <- atomically $ readTChan chan
  putStrLn $ "Loaded: " ++ (show $ counter state) ++ " records, " ++
    (show . size . cache) state ++ " of them are unique"
  dumper chan

job :: Dump -> Conf -> StateRef -> IO ()
job dump conf stateRef = do
  q <- loadQuote
  r <- atomically $ do
    next <- addQuote conf stateRef q
    M.when (next == Continue) $ do
      s <- readTVar stateRef
      writeTChan dump s
    return next
  case r of
    Quit     -> return ()
    Continue -> job dump conf stateRef

loadQuote :: IO Quote
loadQuote = do
  bytes <- simpleHttp url
  let
    text = toString bytes
    doc  = readString [withParseHTML yes, withWarnings yes] text
  quotes <- runX $ doc >>> css "div[class='fi_text']" >>> deep getText
  return $ intercalate " " quotes

nextStep :: Conf -> State -> GrabberState
nextStep c s = if (counter s) >= (maxDupes c)
               then Quit
               else Continue

addQuote :: Conf -> StateRef -> Quote -> STM GrabberState
addQuote conf sr q = do
  s <- readTVar sr
  let newState = updateState q s
  writeTVar sr newState
  return $ nextStep conf newState

updateState :: Quote -> State -> State
updateState quote state =
  if duplicate
  then state { duplicates = (duplicates state) + 1,
               counter = (counter state) + 1 }
  else state { cache = (insert quote oldCache),
               duplicates = 0,
               counter = (counter state) + 1 }
  where
    duplicate = member quote oldCache
    newCache = insert quote oldCache
    oldCache = cache state
