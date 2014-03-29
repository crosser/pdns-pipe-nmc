module PowerDns ( RRType(..)
                , PdnsRequest(..)
                , pdnsParse
                , pdnsOut
                ) where

import NmcJson

data RRType = RRTypeSRV   | RRTypeA   | RRTypeAAAA | RRTypeCNAME
            | RRTypeDNAME | RRTypeSOA | RRTypeRP   | RRTypeLOC
            | RRTypeNS    | RRTypeDS
            | RRTypeANY   | RRTypeError String 
        deriving (Show)

data PdnsRequest = PdnsRequestQ
                   { qName              :: String
                   , qType              :: RRType
                   , iD                 :: String
                   , remoteIpAddress    :: String
                   , localIpAddress     :: Maybe String
                   , ednsSubnetAddress  :: Maybe String
                   }
                 | PdnsRequestAXFR String
                 | PdnsRequestPing
        deriving (Show)

pdnsParse ver s =
  let
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
      "AXFR":x:[]               -> Right (PdnsRequestAXFR x)
      "Q":qn:"IN":qt:id:rip:xs  -> Right (PdnsRequestQ
                                            { qName = qn
                                            , qType = getQt qt
                                            , iD = id
                                            , remoteIpAddress = rip
                                            , localIpAddress = getLIp ver xs
                                            , ednsSubnetAddress = getRIp ver xs
                                            })
      _                         -> Left $ "Unparseable PDNS Request: " ++ s

pdnsOut :: Int -> RRType -> Either String NmcDom -> String
pdnsOut ver rrtype edom =
  case edom of
    Left  err -> "LOG Error: " ++ err ++ "\nFAIL\n"
    Right dom -> "DATA\n" ++ (show dom) ++ "\nEND\n" --FIXME
