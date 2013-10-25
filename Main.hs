{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable #-}
import System.Console.CmdArgs.Implicit

defaultUrl = "http://vpustotu.ru/moderation/"

data Config = Config { url        :: String
                     , maxThreads :: Int
                     , dumpPeriod :: Int
                     } deriving (Show, Data, Typeable)

config = Config { url = defaultUrl &= help "URL to grab from"
                , maxThreads = 5 &= help "Max threads"
                , dumpPeriod = 2 &= help "Show progress every N seconds"
                } &= summary "Quotes Grabber"

main = print =<< cmdArgs config
