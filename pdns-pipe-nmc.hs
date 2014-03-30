{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import Control.Monad
import qualified Data.ByteString.Char8 as C (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as L (pack, unpack)
import Data.ByteString.Lazy as BS hiding (reverse, putStr, putStrLn)
import Data.List.Split
import Data.Aeson (encode, decode, Value(..))
import Network.HTTP.Types
import Data.Conduit
import Network.HTTP.Conduit

import JsonRpcClient
import Config
import PowerDns
import NmcJson

confFile = "/etc/namecoin.conf"

-- HTTP/JsonRpc interface

qReq :: Config -> ByteString -> ByteString -> Request m
qReq cf q id = applyBasicAuth (C.pack (rpcuser cf)) (C.pack (rpcpassword cf))
             $ def { host           = (C.pack (rpchost cf))
                   , port           = (rpcport cf)
                   , method         = "PUT"
                   , requestHeaders = [ (hAccept,      "application/json")
                                      , (hContentType, "application/json")
                                      , (hConnection,  "Keep-Alive")
                                      ]
                   , requestBody    = RequestBodyLBS $ encode $
                                      JsonRpcRequest JsonRpcV1
                                                     "name_show"
                                                     [q]
                                                     (String "pdns-nmc")
                   , checkStatus    = \_ _ _ -> Nothing
                   }

qRsp :: Response ByteString -> Either String NmcDom
qRsp rsp =
    case parseJsonRpc (responseBody rsp) :: Either JsonRpcError NmcRes of
      Left  jerr -> 
        case (jrpcErrCode jerr) of
          -4 -> Right emptyNmcDom
          _  -> Left $ "JsonRpc error response: " ++ (show jerr)
      Right jrsp ->
        case resValue jrsp of
          "" -> Right emptyNmcDom
          vstr ->
            case decode vstr :: Maybe NmcDom of
              Nothing  -> Left $ "Unparseable value: " ++ (show vstr)
              Just dom -> Right dom

-- NMC interface

queryNmc :: Manager -> Config -> String -> String
         -> IO (Either String NmcDom)
queryNmc mgr cfg fqdn qid = do
  case reverse (splitOn "." fqdn) of
    "bit":dn:xs -> do
      rsp <- runResourceT $
             httpLbs (qReq cfg (L.pack ("d/" ++ dn)) (L.pack qid)) mgr
      return $ case qRsp rsp of
        Left  err -> Left err
        Right dom -> Right $ descendNmc xs dom
    _           ->
      return $ Left "Only \".bit\" domain is supported"

-- Main entry

main = do

  cfg <- readConfig confFile

  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
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

  mgr <- newManager def
  forever $ do
    l <- getLine
    case pdnsParse ver l of
      Left e -> putStr $ pdnsReport e
      Right preq -> do
        case preq of
          PdnsRequestQ qname qtype id _ _ _ ->
            queryNmc mgr cfg qname id >>= putStr . (pdnsOut ver id qname qtype)
          PdnsRequestAXFR xfrreq ->
            putStr $ pdnsReport ("No support for AXFR " ++ xfrreq)
          PdnsRequestPing -> putStrLn "END"

-- for testing

ask str = do
  cfg <- readConfig confFile
  mgr <- newManager def
  queryNmc mgr cfg str "askid" >>= putStr . (pdnsOut 1 "askid" str RRTypeANY)
