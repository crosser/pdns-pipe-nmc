module PowerDns ( RRType(..)
                , PdnsRequest(..)
                , pdnsParse
                , pdnsReport
                , pdnsOut
                ) where

import NmcDom

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

pdnsReport :: String -> String
pdnsReport err =
  "LOG\tError: " ++ err ++ "\nFAIL\n"

pdnsOut :: Int -> String -> String -> RRType -> Either String NmcDom -> String
pdnsOut ver id name rrtype edom =
  case edom of
    Left  err -> pdnsReport err
    Right dom -> foldr addLine "END\n" $ nmc2pdns name rrtype dom
      where
        addLine (nm, ty, dt) accum =
          "DATA\t" ++ v3ext ++ nm ++ "\tIN\t" ++ ty ++ "\t" ++ ttl ++
              "\t" ++ id ++ "\t" ++ dt ++ "\n" ++ accum
        v3ext = case ver of
          3 -> "0\t1\t"
          _ -> ""
        ttl = show 3600

nmc2pdns :: String -> RRType -> NmcDom -> [(String, String, String)]
nmc2pdns name RRTypeANY   dom =
  foldr (\r accum -> (nmc2pdns name r dom) ++ accum) []
    [RRTypeA, RRTypeAAAA, RRTypeCNAME, RRTypeDNAME, -- no SRV here!
     RRTypeSOA, RRTypeRP, RRTypeLOC, RRTypeNS, RRTypeDS]
nmc2pdns name RRTypeSRV   dom = [] -- FIXME
nmc2pdns name RRTypeA     dom = mapto name "A" $ domIp dom
nmc2pdns name RRTypeAAAA  dom = mapto name "AAAA" $ domIp6 dom
nmc2pdns name RRTypeCNAME dom = takejust name "CNAME" $ domAlias dom
nmc2pdns name RRTypeDNAME dom = takejust name "DNAME" $ domTranslate dom
nmc2pdns name RRTypeSOA   dom =
  if dom == emptyNmcDom then []
  else
    let
      email = case domEmail dom of
        Nothing   -> "hostmaster." ++ name
        Just addr ->
          let (aname, adom) = break (== '@') addr
          in case adom of
            "" -> aname
            _  -> aname ++ "." ++ (tail adom)
    in [(name, "SOA", email ++ " 99999999 10800 3600 604800 86400")]
nmc2pdns name RRTypeRP    dom = [] --FIXME
nmc2pdns name RRTypeLOC   dom = takejust name "LOC" $ domLoc dom
nmc2pdns name RRTypeNS    dom = mapto name "NS" $ domNs dom
nmc2pdns name RRTypeDS    dom = [] --FIXME

mapto name rrstr maybel = case maybel of
  Nothing  -> []
  Just l   -> map (\x -> (name, rrstr, x)) l

takejust name rrstr maybestr = case maybestr of
  Nothing  -> []
  Just str -> [(name, rrstr, str)]
