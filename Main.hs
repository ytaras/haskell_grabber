{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Set
import Control.Concurrent.STM
import Network.HTTP.Conduit      (simpleHttp)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (intercalate)
import Text.XML.HXT.Core
import Text.HandsomeSoup

url = "http://vpustotu.ru/moderation/"

main = do
  stateRef <- atomically $ newTVar emptyState
  q <- loadQuote
  _ <- atomically $ addQuote conf stateRef q
  state <- atomically $ readTVar stateRef
  putStrLn $ (head . elems . cache) state
  print state


type Quote = String
type Cache a = Set a

data Conf = Conf { threads  :: !Int
                 , maxDupes :: !Int
                 }
conf = Conf 1 10

data State = State { cache      :: Cache Quote
                   , duplicates :: !Int
                   } deriving Show
emptyState = State empty 0
type StateRef = TVar State
-- TODO We're overusing state word
data GrabberState = Continue | Quit

loadQuote :: IO Quote
loadQuote = do
  bytes <- simpleHttp url
  let
    text = toString bytes
    doc  = readString [withParseHTML yes, withWarnings no] text
  quotes <- runX $ doc >>> css "div[class='fi_text']" >>> deep getText
  return $ intercalate " " quotes

nextStep :: Conf -> State -> GrabberState
nextStep c s = if (duplicates s) >= (maxDupes c)
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
  then state { duplicates = (duplicates state) + 1}
  else State { cache = (insert quote oldCache), duplicates = 0 }
  where
    duplicate = member quote oldCache
    newCache = insert quote oldCache
    oldCache = cache state
