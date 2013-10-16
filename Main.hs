{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.Set
import Control.Concurrent.STM

url = "http://vpustotu.ru/moderation/"

main = undefined

newtype Quote = Quote String deriving (Ord, Eq, Show)
newtype Cache a = Cache { cachestore :: Set a }

data Conf = Conf { threads  :: !Int
                 , maxDupes :: !Int
                 }

data State = State { cache      :: Cache Quote
                   , duplicates :: !Int
                   }

type StateRef = TVar State
-- TODO We're overusing state word
data GrabberState = Continue | Quit

loadQuote :: IO Quote
loadQuote = undefined

modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ tv f = readTVar tv >>= writeTVar tv . f

nextStep :: State -> GrabberState
nextStep s = if (duplicates s) >= 20 then Quit else Continue

addQuote :: StateRef -> Quote -> STM GrabberState
addQuote sr q = do
  s <- readTVar sr
  let newState = updateState q s
  writeTVar sr newState
  return $ nextStep newState

updateState :: Quote -> State -> State
updateState quote state =
  if duplicate
  then state { duplicates = (duplicates state) + 1}
  else State { cache = Cache (insert quote oldCache), duplicates = 0 }
  where
    duplicate = member quote oldCache
    newCache = Cache $! insert quote oldCache
    oldCache = (cachestore . cache) state
