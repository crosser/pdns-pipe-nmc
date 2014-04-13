{-# LANGUAGE OverloadedStrings #-}

module NmcDom   ( NmcDom(..)
                , emptyNmcDom
                , seedNmcDom
                , descendNmcDom
                ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T (unpack)
import Data.List.Split
import Data.Char
import Data.Map as M (Map, lookup, delete, size)
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
                        , domImport      :: Maybe String
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

-- FIXME -- I hope there exists a better way to merge records!
mergeNmcDom :: NmcDom -> NmcDom -> NmcDom
mergeNmcDom sub dom = dom  { domService = choose domService
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
                        , domMap =         choose domMap
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
queryNmcDom ::
  (String -> IO (Either String ByteString)) -- ^ query operation action
  -> String                                 -- ^ key
  -> IO (Either String NmcDom)              -- ^ error string or domain
queryNmcDom queryOp key = do
  l <- queryOp key
  case l of
    Left estr -> return $ Left estr
    Right str -> case decode str :: Maybe NmcDom of
      Nothing  -> return $ Left $ "Unparseable value: " ++ (show str)
      Just dom -> return $ Right dom

-- | Try to fetch "import" object and merge it into the base domain
--   Original "import" element is removed, but new imports from the
--   imported objects are processed recursively until there are none.
mergeImport ::
  (String -> IO (Either String ByteString)) -- ^ query operation action
  -> NmcDom                                 -- ^ base domain
  -> IO (Either String NmcDom)              -- ^ result with merged import
mergeImport queryOp base = do
  let
    mbase = mergeSelf base
    base' = mbase {domImport = Nothing}
  -- print base
  case domImport mbase of
    Nothing  -> return $ Right base'
    Just key -> do
      sub <- queryNmcDom queryOp key
      case sub of
        Left  e    -> return $ Left e
        Right sub' -> mergeImport queryOp $ sub' `mergeNmcDom` base'

-- | If there is an element in the map with key "", merge the contents
--   and remove this element. Do this recursively.
mergeSelf :: NmcDom -> NmcDom
mergeSelf base =
  let
    map   = domMap base
    base' = base {domMap = removeSelf map}
    removeSelf Nothing    = Nothing
    removeSelf (Just map) = if size map' == 0 then Nothing else Just map'
      where map' = M.delete "" map
  in
    case map of
      Nothing   -> base'
      Just map' ->
        case M.lookup "" map' of
          Nothing  -> base'
          Just sub -> (mergeSelf sub) `mergeNmcDom` base'

-- | Presence of some elements require removal of some others
normalizeDom :: NmcDom -> NmcDom
normalizeDom dom = foldr id dom [ nsNormalizer
                                , translateNormalizer
                                ]
  where
    nsNormalizer dom = case domNs dom of
      Nothing  -> dom
      Just ns  -> emptyNmcDom { domNs = domNs dom, domEmail = domEmail dom }
    translateNormalizer dom = case domTranslate dom of
      Nothing  -> dom
      Just tr  -> dom { domMap = Nothing }

-- | Merge imports and Selfs and follow the maps tree to get dom
descendNmcDom ::
  (String -> IO (Either String ByteString)) -- ^ query operation action
  -> [String]                               -- ^ subdomain chain
  -> NmcDom                                 -- ^ base domain
  -> IO (Either String NmcDom)              -- ^ fully processed result
descendNmcDom queryOp subdom base = do
  base' <- mergeImport queryOp base
  case subdom of
    []   -> return $ fmap normalizeDom base'
    d:ds ->
      case base' of
        Left err     -> return base'
        Right base'' ->
          case domMap base'' of
            Nothing  -> return $ Right emptyNmcDom
            Just map ->
              case M.lookup d map of
                Nothing  -> return $ Right emptyNmcDom
                Just sub -> descendNmcDom queryOp ds sub

-- | Initial NmcDom populated with "import" only, suitable for "descend"
seedNmcDom ::
  String        -- ^ domain key (without namespace prefix)
  -> NmcDom     -- ^ resulting seed domain
seedNmcDom dn = emptyNmcDom { domImport = Just ("d/" ++ dn)}
