{-# LANGUAGE OverloadedStrings #-}

module NmcDom   ( NmcDom(..)
                , NmcRRSrv(..)
                , NmcRRI2p(..)
                , NmcRRTlsa(..)
                , NmcRRDs(..)
                , merge
                ) where

import Prelude hiding (length)
import Control.Applicative ((<$>), (<*>), empty, pure)
import Data.Char
import Data.Text (Text, unpack)
import Data.List (union)
import Data.List.Split
import Data.Vector ((!), length)
import qualified Data.Vector as V (singleton)
import Data.Map (Map, unionWith, foldrWithKey)
import qualified Data.Map as M (singleton, empty, insert, insertWith)
import qualified Data.HashMap.Strict as H (lookup)
import Data.Aeson
import Data.Aeson.Types
import Data.Default.Class

-- Variant of Aeson's `.:?` that interprets a String as a
-- single-element list, so it is possible to have either
--      "ip":["1.2.3.4"]
-- or
--      "ip":"1.2.3.4"
-- with the same result.
(.:/) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:/ key = case H.lookup key obj of
               Nothing -> pure Nothing
               Just v  -> case v of
                        String s -> parseJSON $ Array (V.singleton v)
                        _        -> parseJSON v

data IntRRService = IntRRService { isvName       :: String
                                 , isvProto      :: String
                                 , isvPrio       :: Int
                                 , isvWeight     :: Int
                                 , isvPort       :: Int
                                 , isvHost       :: String
                                 } deriving (Show, Eq)

instance FromJSON IntRRService where
        parseJSON (Array a) =
                if length a == 6 then IntRRService
                        <$> parseJSON (a ! 0)
                        <*> parseJSON (a ! 1)
                        <*> parseJSON (a ! 2)
                        <*> parseJSON (a ! 3)
                        <*> parseJSON (a ! 4)
                        <*> parseJSON (a ! 5)
                else empty
        parseJSON _ = empty

makeMx :: Object -> Parser (Maybe [String])
makeMx o =
  case H.lookup "service" o of
    Nothing          -> pure Nothing
    Just (Array a) -> do
      isvl <- parseJSON (Array a)
      return $ Just $ map mxStr $ filter mxMatch isvl
        where
          mxMatch isv = isvName isv  == "smtp"
                     && isvProto isv == "tcp"
                     && isvPort isv  == 25
          mxStr isv = (show (isvPrio isv)) ++ "\t" ++ (isvHost isv)
    Just _ -> empty

makeSubmap :: Object -> Parser (Maybe (Map String NmcDom))
makeSubmap o = ((.).(.)) merge merge <$> takeTls o <*> takeSrv o <*> takeMap o

takeMap :: Object -> Parser (Maybe (Map String NmcDom))
takeMap o =
  case H.lookup "map" o of
    Nothing          -> pure Nothing
    Just (Object mo) -> do
      unsplit <- (parseJSON (Object mo) :: Parser (Maybe (Map String NmcDom)))
      let result = fmap splitup unsplit
      return result
        where
          splitup :: Map String NmcDom -> Map String NmcDom
          splitup x = foldrWithKey stow M.empty x
          stow fqdn sdom acc = M.insertWith merge fqdn' sdom' acc
            where
              (fqdn', sdom') = nest (filter (/= "") (splitOnDots fqdn), sdom)
              splitOnDots s = splitOn "." s
              nest ([], v)   = (fqdn, v) -- can split result be empty?
              nest ([k], v)  = (k, v)
              nest (k:ks, v) =
                nest (ks, def { domSubmap = Just (M.singleton k v) })
    _                -> empty

takeSrv :: Object -> Parser (Maybe (Map String NmcDom))
takeSrv o =
  case H.lookup "service" o of
    Nothing          -> pure Nothing
    Just (Array a) -> do
      isvl <- parseJSON (Array a)
      return $ foldr addSrv (Just M.empty) isvl
        where
          addSrv isv acc = subm `merge` acc
            where
              subm = Just (M.singleton ("_" ++ isvProto isv) sub2)
              sub2 = def { domSubmap =
                             Just (M.singleton ("_" ++ isvName isv) sub3) }
              sub3 = def { domSrv = Just [ NmcRRSrv (isvPrio isv)
                                                    (isvWeight isv)
                                                    (isvPort isv)
                                                    (isvHost isv) ] }
    Just _ -> empty

-- takeTls is almost, but not quite, entirely unlike takeSrv
takeTls :: Object -> Parser (Maybe (Map String NmcDom))
takeTls o =
  case H.lookup "tls" o of
    Nothing         -> pure Nothing
    Just (Object t) ->
      (parseJSON (Object t) :: Parser (Map String (Map String [NmcRRTlsa])))
        >>= tmap2dmap
          where
            tmap2dmap :: Map String (Map String [NmcRRTlsa])
                      -> Parser (Maybe (Map String NmcDom))
                -- FIXME return parse error on invalid proto or port
            tmap2dmap m1 = return $ foldrWithKey addprotoelem (Just M.empty) m1
            addprotoelem k1 m2 acc = protoelem k1 m2 `merge` acc
            protoelem k1 m2 = Just (M.singleton ("_" ++ k1) (pmap2dmap m2))
            pmap2dmap m2 = foldrWithKey addportelem def m2
            addportelem k2 v acc = portelem k2 v `merge` acc
            portelem k2 v =
              def { domSubmap = Just (M.singleton ("_" ++ k2)
                                      def { domTlsa = Just v }) }
    Just _ -> empty

class Mergeable a where
        merge :: a -> a -> a -- bias towads second arg

instance (Ord k, Mergeable a) => Mergeable (Map k a) where
        merge mx my = unionWith merge my mx

-- Alas, the following is not possible in Haskell :-(
-- instance Mergeable String where
--         merge _ b = b

instance Mergeable Value where
        merge _ b = b

instance Mergeable a => Mergeable (Maybe a) where
        merge (Just x) (Just y) = Just (merge x y)
        merge Nothing  (Just y) = Just y
        merge (Just x) Nothing  = Just x
        merge Nothing  Nothing  = Nothing

instance Eq a => Mergeable [a] where
        merge xs ys = union xs ys

data NmcRRSrv = NmcRRSrv
                        { srvPrio       :: Int
                        , srvWeight     :: Int
                        , srvPort       :: Int
                        , srvHost       :: String
                        } deriving (Show, Eq)

instance Mergeable NmcRRSrv where
        merge _ b = b

data NmcRRI2p = NmcRRI2p
                        { i2pDestination :: Maybe String
                        , i2pName        :: Maybe String
                        , i2pB32         :: Maybe String
                        } deriving (Show, Eq)

instance FromJSON NmcRRI2p where
        parseJSON (Object o) = NmcRRI2p
                <$> o .:? "destination"
                <*> o .:? "name"
                <*> o .:? "b32"
        parseJSON _ = empty

instance Mergeable NmcRRI2p where
        merge _ b = b

data NmcRRTlsa = NmcRRTlsa
                        { tlsMatchType  :: Int -- 0:exact 1:sha256 2:sha512
                        , tlsMatchValue :: String
                        , tlsIncSubdoms :: Int -- 1:enforce on subdoms 0:no
                        } deriving (Show, Eq)

instance FromJSON NmcRRTlsa where
        parseJSON (Array a) =
                if length a == 3 then NmcRRTlsa
                        <$> parseJSON (a ! 0)
                        <*> parseJSON (a ! 1)
                        <*> parseJSON (a ! 2)
                else empty
        parseJSON _ = empty

instance Mergeable NmcRRTlsa where
        merge _ b = b

data NmcRRDs = NmcRRDs
                        { dsKeyTag      :: Int
                        , dsAlgo        :: Int
                        , dsHashType    :: Int
                        , dsHashValue   :: String
                        } deriving (Show, Eq)

instance FromJSON NmcRRDs where
        parseJSON (Array a) =
                if length a == 4 then NmcRRDs
                        <$> parseJSON (a ! 0)
                        <*> parseJSON (a ! 1)
                        <*> parseJSON (a ! 2)
                        <*> parseJSON (a ! 3)
                else empty
        parseJSON _ = empty

instance Mergeable NmcRRDs where
        merge _ b = b

data NmcDom = NmcDom    { domIp          :: Maybe [String]
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
                        , domDelegate    :: Maybe String
                        , domImport      :: Maybe [String]
                        , domSubmap      :: Maybe (Map String NmcDom)
                        , domFingerprint :: Maybe [String]
                        , domDs          :: Maybe [NmcRRDs]
                        , domMx          :: Maybe [String]    -- Synthetic
                        , domSrv         :: Maybe [NmcRRSrv]  -- Synthetic
                        , domTlsa        :: Maybe [NmcRRTlsa] -- Synthetic
                        } deriving (Show, Eq)

instance Default NmcDom where
  def = NmcDom Nothing Nothing Nothing Nothing Nothing Nothing Nothing
               Nothing Nothing Nothing Nothing Nothing Nothing Nothing
               Nothing Nothing Nothing Nothing Nothing

instance FromJSON NmcDom where
        -- Wherever we expect a domain object, there may be a string
        -- containing IPv4 address. Interpret it as such.
        -- Question: shall we try to recognize IPv6 addresses too?
        parseJSON (String s) =
                 return $ if isIPv4 s'
                            then def { domIp = Just [s'] }
                            else def
                          where
                            s' = unpack s
                            isIPv4 x = all isNibble $ splitOn "." x
                            isNibble x =
                              if all isDigit x then (read x :: Int) < 256
                              else False
        parseJSON (Object o) = NmcDom
                <$> o .:/ "ip"
                <*> o .:/ "ip6"
                <*> o .:? "tor"
                <*> o .:? "i2p"
                <*> o .:? "freenet"
                <*> o .:? "alias"
                <*> o .:? "translate"
                <*> o .:? "email"
                <*> o .:? "loc"
                <*> o .:? "info"
                <*> o .:/ "ns"
                <*> o .:? "delegate"
                <*> o .:/ "import"
                <*> makeSubmap o
                <*> o .:/ "fingerprint"
                <*> o .:? "ds"
                <*> makeMx o
                <*> return Nothing -- domSrv created in subdomains
                <*> return Nothing -- domTlsa created in subdomains
        parseJSON _ = empty

instance Mergeable NmcDom where
        merge sub dom = dom     { domIp =          mergelm domIp
                                , domIp6 =         mergelm domIp6
                                , domTor =         choose  domTor
                                , domI2p =         mergelm domI2p
                                , domFreenet =     choose  domFreenet
                                , domAlias =       choose  domAlias
                                , domTranslate =   choose  domTranslate
                                , domEmail =       choose  domEmail
                                , domLoc =         choose  domLoc
                                , domInfo =        mergelm domInfo
                                , domNs =          mergelm domNs
                                , domDelegate =    mergelm domDelegate
                                , domImport =      mergelm domImport
                                , domSubmap =      mergelm domSubmap
                                , domFingerprint = mergelm domFingerprint
                                , domDs =          mergelm domDs
                                , domMx =          mergelm domMx
                                , domSrv =         mergelm domSrv
                                , domTlsa =        mergelm domTlsa
                                }
          where
                mergelm x = merge (x sub) (x dom)
-- Because it is not possible to define instance of merge for Strings,
-- we have to treat string elements separately, otherwise strings are
-- 'unioned' along with the rest of lists. Ugly, but alternatives are worse.
                choose field = case field dom of
                        Nothing -> field sub
                        Just x  -> Just x
