module PowerDns ( RRType(..)
                , PdnsRequest(..)
                , pdnsParse
                , pdnsReport
                , pdnsOut
                ) where

import NmcDom

data RRType = RRTypeSRV   | RRTypeA   | RRTypeAAAA | RRTypeCNAME
            | RRTypeDNAME | RRTypeSOA | RRTypeRP   | RRTypeLOC
            | RRTypeNS    | RRTypeDS  | RRTypeMX
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
pdnsOut ver id name rrtype edom = case edom of
  Left  err -> pdnsReport $ err ++ " in a query for " ++ name
  Right dom -> foldr addLine "END\n" $ n2p rrtype
    where
      addLine (nm, ty, dt) accum =
        "DATA\t" ++ v3ext ++ nm ++ "\tIN\t" ++ ty ++ "\t" ++ ttl ++
            "\t" ++ id ++ "\t" ++ dt ++ "\n" ++ accum
      v3ext = case ver of
        3 -> "0\t1\t"
        _ -> ""
      ttl = show 3600

      n2p RRTypeANY   =
        foldr (\r accum -> (n2p r) ++ accum) []
          [RRTypeSRV, RRTypeA, RRTypeAAAA, RRTypeCNAME, RRTypeDNAME,
           RRTypeRP, RRTypeLOC, RRTypeNS, RRTypeDS, RRTypeMX]
      n2p RRTypeSRV   = mapto    "SRV"   $ domSrv dom
      n2p RRTypeMX    = mapto    "MX"    $ domMx dom
      n2p RRTypeA     = mapto    "A"     $ domIp dom
      n2p RRTypeAAAA  = mapto    "AAAA"  $ domIp6 dom
      n2p RRTypeCNAME = takejust "CNAME" $ domAlias dom
      n2p RRTypeDNAME = takejust "DNAME" $ domTranslate dom
      n2p RRTypeSOA   = -- FIXME generate only for top domain
                        -- FIXME make realistic version field
                        -- FIXME make realistic nameserver field
        if dom == emptyNmcDom then []
        else [(name, "SOA", "ns " ++ email ++ " 99999 10800 3600 604800 86400")]
          where
            email = case domEmail dom of
              Nothing   -> "hostmaster." ++ name
              Just addr -> dotmail addr
      n2p RRTypeRP    = case domEmail dom of
        Nothing   -> []
        Just addr -> [(name, "RP", (dotmail addr) ++ " .")]
      n2p RRTypeLOC   = takejust "LOC"  $ domLoc dom
      n2p RRTypeNS    = mapto    "NS"   $ domNs dom
      n2p RRTypeDS    = case domDs dom of
        Nothing  -> []
        Just dss -> map (\x -> (name, "DS", dsStr x)) dss
          where
            dsStr x = (show (dsKeyTag x)) ++ " "
                   ++ (show (dsAlgo x)) ++ " "
                   ++ (show (dsHashType x)) ++ " "
                   ++ (dsHashValue x)

      dotmail addr = 
        let (aname, adom) = break (== '@') addr
        in case adom of
          "" -> aname
          _  -> aname ++ "." ++ (tail adom)
      
      mapto    rrstr maybel   = case maybel of
        Nothing  -> []
        Just l   -> map (\x -> (name, rrstr, x)) l
      takejust rrstr maybestr = case maybestr of
        Nothing  -> []
        Just str -> [(name, rrstr, str)]
