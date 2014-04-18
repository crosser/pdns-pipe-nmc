module PowerDns ( RRType(..)
                , PdnsRequest(..)
                , pdnsParse
                , pdnsReport
                , pdnsOut
                , pdnsOutXfr
                ) where

import Data.Text.Lazy (splitOn, pack)

import NmcDom

data RRType = RRTypeSRV   | RRTypeA   | RRTypeAAAA | RRTypeCNAME
            | RRTypeDNAME | RRTypeSOA | RRTypeRP   | RRTypeLOC
            | RRTypeNS    | RRTypeDS  | RRTypeMX
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
  show RRTypeANY       = "ANY"
  show (RRTypeError s) = "RR type error: " ++ (show s)

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
      _        -> -1
    getQt qt = case qt of
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
      "ANY"     -> RRTypeANY
      _         -> RRTypeError qt
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
      "Q":qn:"IN":qt:id:rip:xs  -> Right (PdnsRequestQ
                                            { qName = qn
                                            , qType = getQt qt
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
pdnsOut :: Int -> Int -> String -> RRType -> Either String NmcDom -> String
pdnsOut ver id name rrtype edom = case edom of
  Left  err ->
    pdnsReport $ err ++ " in a " ++ (show rrtype) ++ "query for " ++ name
  Right dom ->
    case rrtype of
      RRTypeANY -> foldr (\x a -> (formatRR ver id name dom x) ++ a) "END\n"
          [RRTypeSRV, RRTypeA, RRTypeAAAA, RRTypeCNAME, RRTypeDNAME,
           RRTypeRP, RRTypeLOC, RRTypeNS, RRTypeDS, RRTypeMX]
      _         -> (formatRR ver id name dom rrtype) ++ "END\n"

-- | Produce answer to the AXFR request
pdnsOutXfr :: Int -> Int -> String -> Either String NmcDom -> String
pdnsOutXfr ver id name edom = "" -- FIXME

justl accessor _ dom = case accessor dom of
  Nothing -> []
  Just xs -> xs

justv accessor _ dom = case accessor dom of
  Nothing -> []
  Just x  -> [x]

dotmail addr = 
  let (aname, adom) = break (== '@') addr
  in case adom of
    "" -> aname
    _  -> aname ++ "." ++ (tail adom)

dataRR RRTypeSRV   = justl domSrv
dataRR RRTypeMX    = justl domMx
dataRR RRTypeA     = justl domIp
dataRR RRTypeAAAA  = justl domIp6
dataRR RRTypeCNAME = justv domAlias
dataRR RRTypeDNAME = justv domTranslate
dataRR RRTypeSOA   = soa
  where
    soa name dom  =     -- FIXME generate only for top domain
                        -- FIXME make realistic version field
                        -- FIXME make realistic nameserver field
  -- Follows a relatively ugly hack to figure if we are at the top
  -- level domain ("something.bit"). Only in such case we provide
  -- the synthetic SOA RR. Otherwise yield empty.
      case splitOn (pack ".") (pack name) of
        [_,_] ->
          if dom == emptyNmcDom then []
          else ["ns " ++ email ++ " 99999 10800 3600 604800 86400"]
            where
              email = case domEmail dom of
                Nothing   -> "hostmaster." ++ name
                Just addr -> dotmail addr
        _ -> []
dataRR RRTypeRP    = rp
  where
    rp _ dom = case domEmail dom of
      Nothing   -> []
      Just addr -> [(dotmail addr) ++ " ."]
dataRR RRTypeLOC   = justv domLoc
dataRR RRTypeNS    = justl domNs
dataRR RRTypeDS    = ds
  where
    ds _ dom = case domDs dom of
      Nothing  -> []
      Just dss -> map dsStr dss
        where
          dsStr x = (show (dsKeyTag x)) ++ " "
                 ++ (show (dsAlgo x)) ++ " "
                 ++ (show (dsHashType x)) ++ " "
                 ++ (dsHashValue x)

formatRR ver id name dom rrtype =
  foldr (\x a -> "DATA\t" ++ v3ext ++ name ++ "\tIN\t" ++ (show rrtype)
            ++ "\t" ++ ttl ++ "\t" ++ (show id) ++ "\t" ++ x ++ "\n" ++ a)
        "" $ dataRR rrtype name dom
    where
      v3ext = case ver of
        3 -> "0\t1\t"
        _ -> ""
      ttl = show 3600
