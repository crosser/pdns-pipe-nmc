{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (lookup, readFile)
import System.Environment
import System.IO hiding (readFile)
import System.IO.Error
import Control.Exception
import Text.Show.Pretty hiding (String)
import Control.Monad
import Control.Monad.State
import Data.ByteString.Lazy hiding (reverse, putStr, putStrLn, head, empty)
import qualified Data.ByteString.Char8 as C (pack)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import qualified Data.Text as T (pack)
import Data.List.Split
import Data.Map.Lazy (Map, empty, lookup, insert, delete, size)
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

qReq :: Config -> String -> Int -> Request m
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
                                                     (String (T.pack (show id)))
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

  let
    fetch = lookup
    -- Save the name under current count, increment count for the next run
    -- so the name is saved under the count that was put into the response.
    stow name (count, cache) =
      (if count >= 99 then 0 else count + 1
      , insert count name
          $ delete (if count >= 10 then count - 10 else count + 90) cache
      )
    io = liftIO

    mainloop = forever $ do
      l <- io getLine
      (count, cache) <- get
      case pdnsParse ver l of
        Left e -> io $ putStr $ pdnsReport e
        Right preq -> do
          case preq of
            PdnsRequestQ qname qtype id _ _ _ -> do
              io $ queryDom (queryOpNmc cfg mgr id) qname
                     >>= putStr . (pdnsOut ver count qname qtype)
  -- debug
              io $ putStrLn $ "LOG\tRequest number " ++ (show count)
                           ++ " id: " ++ (show id)
                           ++ " qname: " ++ qname
                           ++ " qtype: " ++ (show qtype)
                           ++ " cache size: " ++ (show (size cache))
  -- end debug
              put $ stow qname (count, cache)
            PdnsRequestAXFR xrq ->
              case fetch xrq cache of
                Nothing ->
                  io $ putStr $
                    pdnsReport ("AXFR for unknown id: " ++ (show xrq))
                Just qname ->
                  io $ queryDom (queryOpNmc cfg mgr xrq) qname
                    >>= putStr . (pdnsOutXfr ver count qname)
            PdnsRequestPing -> io $ putStrLn "END"

  runStateT mainloop (0, empty) >> return ()

-- query by key from Namecoin

mainOne key qt = do
  cfg <- readConfig confFile
  mgr <- newManager def
  dom <- queryDom (queryOpNmc cfg mgr (-1)) key
  putStrLn $ ppShow dom
  putStr $ pdnsOut 1 (-1) key qt dom

-- using file backend for testing json domain data

mainFile key qt = do
  dom <- queryDom queryOpFile key
  putStrLn $ ppShow dom
  putStr $ pdnsOut 1 (-1) key qt dom

-- Entry point

main = do
  args <- getArgs
  case args of
    []                 -> mainPdnsNmc
    [key, qtype]       -> mainOne key (rrType qtype)
    ["-f" ,key, qtype] -> mainFile key (rrType qtype)
    _ -> error $ "usage: empty args, or \"[-f] <fqdn> <QTYPE>\" (type in caps)"
