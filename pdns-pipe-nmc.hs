{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)
import System.Environment
import System.IO hiding (readFile)
import System.IO.Error
import Control.Exception
import Text.Show.Pretty hiding (String)
import Control.Monad
import Data.ByteString.Lazy hiding (reverse, putStr, putStrLn, head)
import qualified Data.ByteString.Char8 as C (pack)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import qualified Data.Text as T (pack)
import Data.List.Split
import Data.Aeson (encode, decode, Value(..))
import Network.HTTP.Types
import Data.Conduit
import Network.HTTP.Conduit

import JsonRpcClient
import Config
import PowerDns
import NmcRpc
import NmcDom
import NmcTransform

confFile = "/etc/namecoin.conf"

-- HTTP/JsonRpc interface

qReq :: Config -> String -> String -> Request m
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
                                                     [L.pack q]
                                                     (String (T.pack id))
                   , checkStatus    = \_ _ _ -> Nothing
                   }

qRsp :: Response ByteString -> Either String ByteString
qRsp rsp =
    case parseJsonRpc (responseBody rsp) :: Either JsonRpcError NmcRes of
      Left  jerr -> 
        case (jrpcErrCode jerr) of
          -4 -> Right "{}"      -- this is how non-existent entry is returned
          _  -> Left $ "JsonRpc error response: " ++ (show jerr)
      Right jrsp -> Right $ resValue jrsp

-- NMC interface

queryOpNmc cfg mgr qid key =
  runResourceT (httpLbs (qReq cfg key qid) mgr) >>= return . qRsp

queryOpFile key = catch (readFile key >>= return . Right)
                        (\e -> return (Left (show (e :: IOException))))

queryDom queryOp fqdn =
  case reverse (splitOn "." fqdn) of
    "bit":dn:xs -> descendNmcDom queryOp xs $ seedNmcDom dn
    _           -> return $ Left "Only \".bit\" domain is supported"

-- Main entries

mainPdnsNmc = do

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
            queryDom (queryOpNmc cfg mgr id) qname >>= putStr . (pdnsOut ver id qname qtype)
          PdnsRequestAXFR xfrreq ->
            putStr $ pdnsReport ("No support for AXFR " ++ xfrreq)
          PdnsRequestPing -> putStrLn "END"

-- query by key from Namecoin

mainOne key = do
  cfg <- readConfig confFile
  mgr <- newManager def
  dom <- queryDom (queryOpNmc cfg mgr "_") key
  putStrLn $ ppShow dom
  putStr $ pdnsOut 1 "_" key RRTypeANY dom

-- using file backend for testing json domain data

mainFile key = do
  dom <- queryDom queryOpFile key
  putStrLn $ ppShow dom
  putStr $ pdnsOut 1 "+" key RRTypeANY dom

-- Entry point

main = do
  args <- getArgs
  case args of
    []         -> mainPdnsNmc
    [key]      -> mainOne key
    ["-f",key] -> mainFile key
    _ -> error $ "usage: empty args, or \"[-f] <fqdn>\""
