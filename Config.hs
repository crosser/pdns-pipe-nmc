module Config ( Config(..)
              , readConfig
              ) where

import Data.ConfigFile
import Data.Either.Utils
import Data.List.Split

data Config = Config { rpcuser       :: String
                     , rpcpassword   :: String
                     , rpchost       :: String
                     , rpcport       :: Int
                     } deriving (Show)

readConfig :: String -> IO Config
readConfig f = do
  cp <- return . forceEither =<< readfile emptyCP f
  return (Config { rpcuser       = getSetting cp "rpcuser"     ""
                 , rpcpassword   = getSetting cp "rpcpassword" ""
                 , rpchost       = getSetting cp "rpchost"     "localhost"
                 , rpcport       = getSetting cp "rpcport"     8336
                 })
    where
      getSetting cp x dfl = case get cp "DEFAULT" x of
                              Left  _ -> dfl
                              Right x -> x

