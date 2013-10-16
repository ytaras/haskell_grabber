{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Set
import Control.Concurrent.STM
import Network.HTTP.Conduit      (simpleHttp)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (intercalate)
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Control.Monad

url = "http://vpustotu.ru/moderation/"

main = do
  stateRef <- atomically $ newTVar emptyState
  _ <- job conf stateRef
  state <- atomically $ readTVar stateRef
  mapM_ putStrLn $ (elems . cache) state
  print $ duplicates state
  print $ counter state
  print $ (size . cache) state


type Quote = String
type Cache a = Set a

data Conf = Conf { threads  :: !Int
                 , maxDupes :: !Int
                 }
conf = Conf 1 50

data State = State { cache      :: Cache Quote
                   , duplicates :: !Int
                   , counter    :: !Int
                   } deriving Show
emptyState = State empty 0 0
type StateRef = TVar State
-- TODO We're overusing state word
data GrabberState = Continue | Quit

job :: Conf -> StateRef -> IO ()
job conf stateRef = do
  q <- loadQuote
  next <- atomically $ addQuote conf stateRef q
  case next of
    Quit     -> return ()
    Continue -> job conf stateRef

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
