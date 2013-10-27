{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable #-}

import Control.Applicative
import Control.Arrow
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (catMaybes)
import Data.Serialize
import Data.String (fromString)
import qualified Data.ByteString as B
import Data.Hashable
import qualified Data.Set as S
import System.Console.CmdArgs.Implicit
import System.IO
import Text.HandsomeSoup
import Text.XML.HXT.Core

defaultUrl = "http://vpustotu.ru/moderation/"

type Quote = B.ByteString
data Config = Config { url           :: String
                     , threads       :: Int
                     , maxDuplicates :: Int
                     , dumpPeriod    :: Int
                     , outFile       :: String
                     , hashFile      :: String
                     } deriving (Show, Data, Typeable)



data ProgramState = Running | Stopping deriving Eq

data Runtime = Runtime { hashes     :: TVar (S.Set Int)
                       , duplicates :: TVar (Int)
                       , state      :: TVar (ProgramState)
                       }

config = Config { url = defaultUrl &= help "URL to grab from"
                , threads = 5 &= help "Max threads"
                , maxDuplicates = 200 &= explicit &= name "duplicates" &= name "d"
                                  &= help "Duplicates threshold to stop"
                , dumpPeriod = 2 &= explicit &= name "showEvery" &= name "s"
                               &= help "Show progress every N seconds"
                , outFile = "quotes.txt" &= typFile &= help "Output file"
                , hashFile = "hash.bin" &= typFile &= help "File to save program state between runs"
                } &= summary "Quotes Grabber"

main = do
  c <- cmdArgs config
  r <- atomically $ prepareRuntime
  startWorkers (threads config) $ whileRunning r $ job c r
  saveState r c

runtimeValue :: (Runtime -> TVar a) -> Runtime -> IO a
runtimeValue f = atomically . readTVar . f

prepareRuntime = Runtime <$> newTVar S.empty
                 <*> newTVar 0 <*> newTVar Running

startWorkers :: Int -> IO () -> IO ()
startWorkers count job = do
  counter <- atomically $ newTVar count
  replicateM_ count $ run counter
  atomically $ waitForZero counter
  where
    run c = forkIO $ job `finally` (atomReplace c (\x -> x - 1))
    atomReplace c = atomically . modifyTVar' c
    waitForZero c = readTVar c >>= \x ->
      if x == 0 then return () else retry

whileRunning :: Runtime -> IO () -> IO ()
whileRunning r j =
  j >>
  runtimeValue state r >>= \s ->
  if s == Running then whileRunning r j else return ()

increment = flip modifyTVar' (+1)
add q = flip modifyTVar' (S.insert q)
isDuplicate q v = readTVar v >>= \s -> return $ S.member q s
checkConditions :: Config -> Runtime -> STM ()
checkConditions c r = do
  dups <- readTVar $ duplicates r
  if dups >= (maxDuplicates c)
    then writeTVar (state r) Stopping
    else return ()

addQuote :: Config -> Runtime -> Quote -> STM (Maybe Quote) -- TODO Is
            -- it better to return bool?
addQuote c r q = do
  let h = hash q
  d <- isDuplicate h $ hashes r
  if d
    then increment $ duplicates r
    else do add h $ hashes r
            reset $ duplicates r
  checkConditions c r
  return $ if d then Nothing else Just q
    where
      reset x = writeTVar x 0

getDoc = fromUrl . url
parseQuote doc = runX $
                 doc >>> css "[class=fi_text]" >>> (deep getText)

loadQuote :: Config -> IO [Quote]
loadQuote c = map fromString <$> (getDoc c >>= parseQuote)

job c r = loadQuote c >>= addQuotes >>= dumpQuotes c
  where
    addQuotes q = atomically $ fmap catMaybes $ mapM (addQuote c r) q


dumpQuotes :: Config -> [Quote]  -> IO ()
dumpQuotes c q = do
  -- TODO open and close file extract out
  h <- openFile (outFile c) AppendMode
  forM_ q $ writeQuote h
  hClose h
  where
    writeQuote h q = B.hPut h q >> hPutStr h "\n\n\n"

saveState :: Runtime -> Config -> IO ()
saveState r c = do
  q <- runtimeValue hashes r
  let v = encode q
  h <- openFile (hashFile c) WriteMode
  B.hPutStr h v
  hClose h
