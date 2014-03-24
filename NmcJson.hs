{-# LANGUAGE OverloadedStrings #-}

module NmcJson  ( NmcRes(..)
                , NmcDom
                ) where

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson

data NmcRRService = NmcRRService -- unused
                        { srvName       :: String
                        , srvProto      :: String
                        , srvW1         :: Int
                        , srvW2         :: Int
                        , srvPort       :: Int
                        , srvHost       :: [String]
                        } deriving (Show)

instance FromJSON NmcRRService where
        parseJSON (Object o) = NmcRRService
                <$> o .: "name"
                <*> o .: "proto"
                <*> o .: "w1"
                <*> o .: "w2"
                <*> o .: "port"
                <*> o .: "host"
        parseJSON _ = empty

data NmcRRI2p = NmcRRI2p -- unused
                        { i2pDestination :: String
                        , i2pName        :: String
                        , i2pB32         :: String
                        } deriving (Show)

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
                        } deriving (Show)

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

data NmcRes = NmcRes    { resName       :: String
                        , resValue      :: ByteString -- NmcDom
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

main = do
  let l = "{\"name\":\"d/dot-bit\",\"value\":\"{\\\"info\\\":{\\\"description\\\":\\\"Dot-BIT Project - Official Website\\\",\\\"registrar\\\":\\\"http://register.dot-bit.org\\\"},\\\"fingerprint\\\":[\\\"30:B0:60:94:32:08:EC:F5:BE:DF:F4:BB:EE:52:90:2C:5D:47:62:46\\\"],\\\"ns\\\":[\\\"ns0.web-sweet-web.net\\\",\\\"ns1.web-sweet-web.net\\\"],\\\"map\\\":{\\\"\\\":{\\\"ns\\\":[\\\"ns0.web-sweet-web.net\\\",\\\"ns1.web-sweet-web.net\\\"]}},\\\"email\\\":\\\"register@dot-bit.org\\\"}\",\"txid\":\"7412603f2e6c3459be56accc6e1f3646b603f3d4a4188119a4072f125c1340d5\",\"address\":\"Mw3KCQcqC44nm75w7r79ZifZbEqT8RetWn\",\"expires_in\":18915}"
  let r = decode l :: Maybe NmcRes
  case r of
    Just resp -> do
      let value = (resValue resp)
      let dom = decode value :: Maybe NmcDom
      print dom
    Nothing   ->
      print "Unparseable NMC response"