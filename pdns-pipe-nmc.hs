{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Exception
import Data.ConfigFile
import Data.Either.Utils
import Data.List.Split
import Data.Aeson (decode)
import Network.JsonRpc.Client
import NmcJson

confFile = "/etc/namecoin.conf"

-- Config file handling

data Config = Config { rpcuser       :: String
                     , rpcpassword   :: String
                     , rpchost       :: String
                     , rpcport       :: String
                     } deriving (Show)

readConfig :: String -> IO Config
readConfig f = do
  cp <- return . forceEither =<< readfile emptyCP f
  return (Config { rpcuser       = getSetting cp "rpcuser"     ""
                 , rpcpassword   = getSetting cp "rpcpassword" ""
                 , rpchost       = getSetting cp "rpchost"     "localhost"
                 , rpcport       = getSetting cp "rpcport"     "8336"
                 })
    where
      getSetting cp x dfl = case get cp "DEFAULT" x of
                              Left  _ -> dfl
                              Right x -> x

uriConf = do
  cfg <- readConfig confFile
  return $ "http://" ++ rpcuser cfg ++ ":" ++ rpcpassword cfg ++
               "@" ++ rpchost cfg ++ ":" ++ rpcport cfg ++ "/"

-- NMC interface

queryNmc :: String -> String -> RRType -> String -> IO (Either String NmcDom)
queryNmc uri fqdn qtype qid = do
  case reverse  (splitOn "." fqdn) of
    "bit":dn:xs -> do
      ans <- detailledRemote Version1 [] uri "name_show" $ "d/" ++ dn
      let mdom = decode (resValue ans) :: Maybe NmcDom
      case mdom of
        Nothing  -> return $ Left ("Unparseable: " ++ (show (resValue ans)))
        Just dom -> return $ Right dom
    _           ->
      return $ Left "Only \".bit\" domain is supported"

-- PowerDNS ABI

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
      _                         -> Left s

pdnsOut :: String -> Either String PdnsRequest -> IO ()
pdnsOut _   (Left e)   = putStrLn ("ERROR\tUnparseable request: " ++ e)
pdnsOut uri (Right rq) = case rq of
    PdnsRequestQ qn qt id lip rip eip -> do
      dom <- queryNmc uri qn qt id
      case dom of
        Left  e      -> putStrLn ("ERROR\tNmc query error: " ++ e)
        Right result -> print result
    PdnsRequestAXFR xfrreq ->
      putStrLn ("ERROR\t No support for AXFR " ++ xfrreq)
    PdnsRequestPing -> putStrLn "OK"

-- Main entry

main = do
  uri <- uriConf
  ver <- do
    let
      loopErr e = forever $ do
        putStrLn $ "FAIL\t" ++ e
        _ <- getLine
        return ()
    s <- getLine
    case words s of
      ["HELO", "1"] -> return 1
      ["HELO", "2"] -> return 2
      ["HELO", "3"] -> return 3
      ["HELO",  x ] -> loopErr $ "unsupported ABI version " ++ (show x)
      _             -> loopErr $ "bad HELO " ++ (show s)

  putStrLn $ "OK\tDnsNmc ready to serve, protocol v." ++ (show ver)
  forever $ getLine >>= (pdnsOut uri) . (pdnsParse ver)
