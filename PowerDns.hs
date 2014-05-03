module PowerDns ( RRType(..)
                , rrType
                , PdnsRequest(..)
                , pdnsParse
                , pdnsReport
                , pdnsOutQ
                , pdnsOutXfr
                ) where

import Data.Text.Lazy (splitOn, pack)
import Data.Map.Lazy (foldrWithKey)
import Data.Default.Class (def)

import NmcDom

data RRType = RRTypeSRV   | RRTypeA   | RRTypeAAAA | RRTypeCNAME
            | RRTypeDNAME | RRTypeSOA | RRTypeRP   | RRTypeLOC
            | RRTypeNS    | RRTypeDS  | RRTypeMX   | RRTypeTLSA
            | RRTypeANY   | RRTypeError String

instance Show RRType where
  show RRTypeSRV       = "SRV"
  show RRTypeA         = "A"
  show RRTypeAAAA      = "AAAA"
  show RRTypeCNAME     = "CNAME"
  show RRTypeDNAME     = "DNAME"
  show RRTypeSOA       = "SOA"
  show RRTypeRP        = "RP"
  show RRTypeLOC       = "LOC"
  show RRTypeNS        = "NS"
  show RRTypeDS        = "DS"
  show RRTypeMX        = "MX"
  show RRTypeTLSA      = "TLSA"
  show RRTypeANY       = "ANY"
  show (RRTypeError s) = "Unknown RR type: " ++ (show s)

rrType qt = case qt of
  "SRV"     -> RRTypeSRV
  "A"       -> RRTypeA
  "AAAA"    -> RRTypeAAAA
  "CNAME"   -> RRTypeCNAME
  "DNAME"   -> RRTypeDNAME
  "SOA"     -> RRTypeSOA
  "RP"      -> RRTypeRP
  "LOC"     -> RRTypeLOC
  "NS"      -> RRTypeNS
  "DS"      -> RRTypeDS
  "MX"      -> RRTypeMX
  "TLSA"    -> RRTypeTLSA
  "ANY"     -> RRTypeANY
  _         -> RRTypeError qt

data PdnsRequest = PdnsRequestQ
                   { qName              :: String
                   , qType              :: RRType
                   , iD                 :: Int
                   , remoteIpAddress    :: String
                   , localIpAddress     :: Maybe String
                   , ednsSubnetAddress  :: Maybe String
                   }
                 | PdnsRequestAXFR Int
                 | PdnsRequestPing
        deriving (Show)

-- | Parse request string read from the core PowerDNS process
pdnsParse :: Int -> String -> Either String PdnsRequest
pdnsParse ver s =
  let
    getInt s = case reads s :: [(Int, String)] of
      [(x, _)] -> x
      _        -> (-1)
    getLIp ver xs
      | ver >= 2  = case xs of
                      x:_       -> Just x
                      _         -> Nothing
      | otherwise = Nothing
    getRIp ver xs
      | ver >= 3  = case xs of
                      _:x:_     -> Just x
                      _         -> Nothing
      | otherwise = Nothing
  in
    case words s of
      "PING":[]                 -> Right PdnsRequestPing
      "AXFR":x:[]               -> Right (PdnsRequestAXFR (getInt x))
      "Q":qn:"IN":qt:id:rip:xs  -> case rrType qt of
                                     RRTypeError e ->
                                       Left $ "Unrecognized RR type: " ++ e
                                     rt ->
                                       Right (PdnsRequestQ
                                            { qName = qn
                                            , qType = rrType qt
                                            , iD = getInt id
                                            , remoteIpAddress = rip
                                            , localIpAddress = getLIp ver xs
                                            , ednsSubnetAddress = getRIp ver xs
                                            })
      _                         -> Left $ "Unparseable PDNS Request: " ++ s

-- | Produce LOG entry followed by FAIL
pdnsReport :: String -> String
pdnsReport err = "LOG\tError: " ++ err ++ "\nFAIL\n"

-- | Produce answer to the Q request
pdnsOutQ :: Int -> Int -> Int -> String -> RRType -> Either String NmcDom -> String
pdnsOutQ ver id gen name rrt edom =
  let
    rrl = case rrt of
      RRTypeANY -> [ RRTypeSRV, RRTypeA, RRTypeAAAA, RRTypeCNAME
                   , RRTypeDNAME, RRTypeRP, RRTypeLOC, RRTypeNS
                   , RRTypeDS, RRTypeMX, RRTypeTLSA -- SOA not included
                   ]
      x         -> [x]
  in
    case edom of
      Left  err ->
        pdnsReport $ err ++ " in the " ++ (show rrt) ++ " query for " ++ name
      Right dom ->
        formatDom ver id gen rrl name dom "END\n"

-- | Produce answer to the AXFR request
pdnsOutXfr :: Int -> Int -> Int -> String -> Either String NmcDom -> String
pdnsOutXfr ver id gen name edom =
  let
    allrrs = [ RRTypeSRV, RRTypeA, RRTypeAAAA, RRTypeCNAME
             , RRTypeDNAME, RRTypeRP, RRTypeLOC, RRTypeNS
             , RRTypeDS, RRTypeMX, RRTypeTLSA, RRTypeSOA
             ]
    walkDom f acc name dom =
      f name dom $ case domSubmap dom of
        Nothing -> acc
        Just dm ->
          foldrWithKey (\n d a -> walkDom f a (n ++ "." ++ name) d) acc dm
  in
    case edom of
      Left  err ->
        pdnsReport $ err ++ " in the AXFR request for " ++ name
      Right dom ->
        walkDom (formatDom ver id gen allrrs) "END\n" name dom

formatDom ver id gen rrl name dom acc =
  foldr (\x a -> (formatRR ver id gen name dom x) ++ a) acc rrl

formatRR ver id gen name dom rrtype =
  foldr (\x a -> "DATA\t" ++ v3ext ++ name ++ "\tIN\t" ++ (show rrtype)
              ++ "\t" ++ ttl ++ "\t" ++ (show id) ++ "\t" ++ x ++ "\n" ++ a)
        "" $ dataRR rrtype gen name dom
    where
      v3ext = case ver of
        3 -> "0\t1\t"
        _ -> ""
      ttl = show 3600

justl accessor _ _ dom = case accessor dom of
  Nothing -> []
  Just xs -> xs

justv accessor _ _ dom = case accessor dom of
  Nothing -> []
  Just x  -> [x]

dotmail addr =
  let (aname, adom) = break (== '@') addr
  in case adom of
    "" -> aname ++ "."
    _  -> aname ++ "." ++ (tail adom) ++ "."

dataRR RRTypeSRV   = \ _ _ dom ->
  case domSrv dom of
    Nothing  -> []
    Just srvs -> map srvStr srvs
      where
        srvStr x = (show (srvPrio x)) ++ "\t"
                ++ (show (srvWeight x)) ++ " "
                ++ (show (srvPort x)) ++ " "
                ++ (srvHost x)
    
dataRR RRTypeMX    = justl domMx
dataRR RRTypeTLSA  = \ _ _ dom ->
  case domTlsa dom of
    Nothing  -> []
    Just tlsas -> map tlsaStr tlsas
      where
        tlsaStr x = "(3 0 "
                 ++ (show (tlsMatchType x)) ++ " "
                 ++ (tlsMatchValue x) ++ ")"
        -- tlsIncSubdoms is not displayed, it is used for `propagate`.

dataRR RRTypeA     = justl domIp
dataRR RRTypeAAAA  = justl domIp6
dataRR RRTypeCNAME = justv domAlias
dataRR RRTypeDNAME = justv domTranslate
dataRR RRTypeSOA   = \ gen name dom ->
  let
    ns = case domNs dom of
      Just (x:_) -> x
      _          -> "."
    email = case domEmail dom of
      Nothing   -> "hostmaster." ++ name ++ "."
      Just addr -> dotmail addr
  in
    if dom == def then []
    else
    -- Follows a relatively ugly hack to figure if we are at the top
    -- level domain ("something.bit"). Only in such case we provide
    -- the synthetic SOA RR. Otherwise yield empty.
    -- Alternative would be to carry "top-ness" as a parameter through
    -- all the calls from the very top where we split the fqdn.
      case splitOn (pack ".") (pack name) of
        [_,_] -> [ns ++ " " ++ email ++ " " ++ (show gen)
                     ++ " 10800 3600 604800 86400"]
        _     -> []
dataRR RRTypeRP    = \ _ _ dom ->
  case domEmail dom of
    Nothing   -> []
    Just addr -> [(dotmail addr) ++ " ."]
dataRR RRTypeLOC   = justv domLoc
dataRR RRTypeNS    = justl domNs
dataRR RRTypeDS    = \ _ _ dom ->
  case domDs dom of
    Nothing  -> []
    Just dss -> map dsStr dss
      where
        dsStr x = (show (dsKeyTag x)) ++ " "
               ++ (show (dsAlgo x)) ++ " "
               ++ (show (dsHashType x)) ++ " "
               ++ (dsHashValue x)
-- This only comes into play when data arrived _not_ from a PDNS request:
dataRR (RRTypeError e) = \ _ _ _ ->
  ["; No data for bad request type " ++ e]
