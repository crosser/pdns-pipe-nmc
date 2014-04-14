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
pdnsOut ver id name rrtype edom =
  case edom of
    Left  err -> pdnsReport $ err ++ " in a query for " ++ name
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
  foldr (\r accum -> (nmc2pdns r) ++ accum) []
    [RRTypeSRV, RRTypeA, RRTypeAAAA, RRTypeCNAME, RRTypeDNAME,
     RRTypeSOA, RRTypeRP, RRTypeLOC, RRTypeNS, RRTypeDS, RRTypeMX]
  where
    nmc2pdns RRTypeSRV   = makesrv  "SRV"   $ domService dom
    nmc2pdns RRTypeMX    = mapto    "MX"    $ domMx dom
    nmc2pdns RRTypeA     = mapto    "A"     $ domIp dom
    nmc2pdns RRTypeAAAA  = mapto    "AAAA"  $ domIp6 dom
    nmc2pdns RRTypeCNAME = takejust "CNAME" $ domAlias dom
    nmc2pdns RRTypeDNAME = takejust "DNAME" $ domTranslate dom
    nmc2pdns RRTypeSOA   = -- FIXME generate only for top domain
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
    nmc2pdns RRTypeRP    = [] --FIXME
    nmc2pdns RRTypeLOC   = takejust "LOC"  $ domLoc dom
    nmc2pdns RRTypeNS    = mapto    "NS"   $ domNs dom
    nmc2pdns RRTypeDS    = [] --FIXME
    
    mapto rrstr maybel = case maybel of
      Nothing  -> []
      Just l   -> map (\x -> (name, rrstr, x)) l
    
    takejust rrstr maybestr = case maybestr of
      Nothing  -> []
      Just str -> [(name, rrstr, str)]
    
    makesrv rrstr mayberl = case mayberl of
      Nothing  -> []
      Just srl  -> map (\x -> (name, rrstr, fmtsrv x)) srl
        where
          fmtsrv rl = (show (srvPrio rl)) ++ " "
                    ++ (show (srvWeight rl)) ++ " "
                    ++ (show (srvPort rl)) ++ " "
                    ++ (srvHost rl)
