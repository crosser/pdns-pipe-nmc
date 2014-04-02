{-# LANGUAGE OverloadedStrings #-}

module NmcJson  ( NmcRes(..)
                , NmcDom(..)
                , emptyNmcDom
                , descendNmc
                ) where

import Data.ByteString.Lazy (ByteString)
import Data.Map as M (Map, lookup)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson

data NmcRRService = NmcRRService -- unused
                        { srvName       :: String
                        , srvProto      :: String
                        , srvW1         :: Int
                        , srvW2         :: Int
                        , srvPort       :: Int
                        , srvHost       :: [String]
                        } deriving (Show, Eq)

instance FromJSON NmcRRService where
        parseJSON (Object o) = NmcRRService
                <$> o .: "name"
                <*> o .: "proto"
                <*> o .: "w1"
                <*> o .: "w2"
                <*> o .: "port"
                <*> o .: "host"
        parseJSON _ = empty

data NmcRRI2p = NmcRRI2p
                        { i2pDestination :: String
                        , i2pName        :: String
                        , i2pB32         :: String
                        } deriving (Show, Eq)

instance FromJSON NmcRRI2p where
        parseJSON (Object o) = NmcRRI2p
                <$> o .: "destination"
                <*> o .: "name"
                <*> o .: "b32"
        parseJSON _ = empty

data NmcDom = NmcDom    { domService     :: Maybe [[String]] -- [NmcRRService]
                        , domIp          :: Maybe [String]
                        , domIp6         :: Maybe [String]
                        , domTor         :: Maybe String
                        , domI2p         :: Maybe NmcRRI2p
                        , domFreenet     :: Maybe String
                        , domAlias       :: Maybe String
                        , domTranslate   :: Maybe String
                        , domEmail       :: Maybe String
                        , domLoc         :: Maybe String
                        , domInfo        :: Maybe Value
                        , domNs          :: Maybe [String]
                        , domDelegate    :: Maybe [String]
                        , domImport      :: Maybe [[String]]
                        , domMap         :: Maybe (Map String NmcDom)
                        , domFingerprint :: Maybe [String]
                        , domTls         :: Maybe (Map String
                                                    (Map String [[String]]))
                        , domDs          :: Maybe [[String]]
                        } deriving (Show, Eq)

instance FromJSON NmcDom where
        parseJSON (Object o) = NmcDom
                <$> o .:? "service"
                <*> o .:? "ip"
                <*> o .:? "ip6"
                <*> o .:? "tor"
                <*> o .:? "i2p"
                <*> o .:? "freenet"
                <*> o .:? "alias"
                <*> o .:? "translate"
                <*> o .:? "email"
                <*> o .:? "loc"
                <*> o .:? "info"
                <*> o .:? "ns"
                <*> o .:? "delegate"
                <*> o .:? "import"
                <*> o .:? "map"
                <*> o .:? "fingerprint"
                <*> o .:? "tls"
                <*> o .:? "ds"
        parseJSON _ = empty

emptyNmcDom = NmcDom Nothing Nothing Nothing Nothing Nothing Nothing
                     Nothing Nothing Nothing Nothing Nothing Nothing
                     Nothing Nothing Nothing Nothing Nothing Nothing

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

normalizeDom :: NmcDom -> NmcDom
normalizeDom dom
  | domNs        dom /= Nothing = emptyNmcDom { domNs    = domNs dom
                                              , domEmail = domEmail dom
                                              }
  | domDelegate  dom /= Nothing = emptyNmcDom -- FIXME
  | domTranslate dom /= Nothing = dom { domMap = Nothing }
  | otherwise                   = dom

descendNmc :: [String] -> NmcDom -> NmcDom
descendNmc subdom rawdom =
  let dom = normalizeDom rawdom
  in case subdom of
    []   ->
      case domMap dom of
        Nothing  -> dom
        Just map ->
          case M.lookup "" map of         -- Stupid, but there are "" in the map
            Nothing  -> dom               -- Try to merge it with the root data
            Just sub -> mergeNmc sub dom  -- Or maybe drop it altogether...
    d:ds ->
      case domMap dom of
        Nothing  -> emptyNmcDom
        Just map ->
          case M.lookup d map of
            Nothing  -> emptyNmcDom
            Just sub -> descendNmc ds sub

-- FIXME -- I hope there exists a better way to merge records!
mergeNmc :: NmcDom -> NmcDom -> NmcDom
mergeNmc sub dom = dom  { domService = choose dom domService sub
                        , domIp = choose dom domIp sub
                        , domIp6 = choose dom domIp6 sub
                        , domTor = choose dom domTor sub
                        , domI2p = choose dom domI2p sub
                        , domFreenet = choose dom domFreenet sub
                        , domAlias = choose dom domAlias sub
                        , domTranslate = choose dom domTranslate sub
                        , domEmail = choose dom domEmail sub
                        , domLoc = choose dom domLoc sub
                        , domInfo = choose dom domInfo sub
                        , domNs = choose dom domNs sub
                        , domDelegate = choose dom domDelegate sub
                        , domImport = choose dom domImport sub
                        , domFingerprint = choose dom domFingerprint sub
                        , domTls = choose dom domTls sub
                        , domDs = choose dom domDs sub
                        }
  where
    choose :: NmcDom -> (NmcDom -> Maybe a) -> NmcDom -> Maybe a
    choose sub t dom = case t dom of
      Nothing -> t sub
      Just x  -> Just x
