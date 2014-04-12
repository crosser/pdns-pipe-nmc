{-# LANGUAGE OverloadedStrings #-}

module NmcDom   ( NmcDom(..)
                , emptyNmcDom
                , descendNmc
                , queryDom
                ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text as T (unpack)
import Data.List.Split
import Data.Char
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
        -- Wherever we expect a domain object, there may be a string
        -- containing IPv4 address. Interpret it as such.
        -- Question: shall we try to recognize IPv6 addresses too?
        parseJSON (String s) =
                 return $ if isIPv4 s'
                            then emptyNmcDom { domIp = Just [s'] }
                            else emptyNmcDom
                          where
                            s' = T.unpack s
                            isIPv4 x = all isNibble $ splitOn "." x
                            isNibble x =
                              if all isDigit x then (read x :: Int) < 256
                              else False
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
mergeNmc sub dom = dom  { domService = choose domService
                        , domIp =          choose domIp
                        , domIp6 =         choose domIp6
                        , domTor =         choose domTor
                        , domI2p =         choose domI2p
                        , domFreenet =     choose domFreenet
                        , domAlias =       choose domAlias
                        , domTranslate =   choose domTranslate
                        , domEmail =       choose domEmail
                        , domLoc =         choose domLoc
                        , domInfo =        choose domInfo
                        , domNs =          choose domNs
                        , domDelegate =    choose domDelegate
                        , domImport =      choose domImport
                        , domFingerprint = choose domFingerprint
                        , domTls =         choose domTls
                        , domDs =          choose domDs
                        }
  where
    choose :: (NmcDom -> Maybe a) -> Maybe a
    choose field = case field dom of
      Nothing -> field sub
      Just x  -> Just x

-- | Perform query and return error string or parsed domain object
queryDom ::
  (ByteString -> IO (Either String ByteString)) -- ^ query operation action
  -> ByteString                                 -- ^ key
  -> IO (Either String NmcDom)                  -- ^ error string or domain
queryDom queryOp key = do
  l <- queryOp key
  case l of
    Left estr -> return $ Left estr
    Right str -> case decode str :: Maybe NmcDom of
      Nothing  -> return $ Left $ "Unparseable value: " ++ (show str)
      Just dom -> return $ Right dom
