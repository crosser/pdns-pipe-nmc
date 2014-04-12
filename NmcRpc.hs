{-# LANGUAGE OverloadedStrings #-}

module NmcRpc   ( NmcRes(..)
                ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text as T (unpack)
import Data.List.Split
import Data.Char
import Data.Map as M (Map, lookup)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson

data NmcRes = NmcRes    { resName       :: String
                        , resValue      :: ByteString -- string with NmcDom
                        , resTxid       :: String
                        , resAddress    :: String
                        , resExpires_in :: Int
                        } deriving (Show)
instance FromJSON NmcRes where
        parseJSON (Object o) = NmcRes
                <$> o .: "name"
                <*> o .: "value"
                <*> o .: "txid"
                <*> o .: "address"
                <*> o .: "expires_in"
        parseJSON _ = empty
