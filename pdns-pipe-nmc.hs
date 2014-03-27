{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as C (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as L (pack, unpack)
import Data.ByteString.Lazy as BS hiding (reverse, putStrLn)
import Data.ConfigFile
import Data.Either.Utils
import Data.List.Split
import Data.Aeson (encode, decode, Value(..))
import Network.HTTP.Types
import Data.Conduit
import Network.HTTP.Conduit
import Data.JsonRpcClient
import PowerDns
import NmcJson

confFile = "/etc/namecoin.conf"

-- Config file handling

data Config = Config { rpcuser       :: String
                     , rpcpassword   :: String
                     , rpchost       :: String
                     , rpcport       :: Int
                     } deriving (Show)

readConfig :: String -> IO Config
readConfig f = do
  cp <- return . forceEither =<< readfile emptyCP f
  return (Config { rpcuser       = getSetting cp "rpcuser"     ""
                 , rpcpassword   = getSetting cp "rpcpassword" ""
                 , rpchost       = getSetting cp "rpchost"     "localhost"
                 , rpcport       = getSetting cp "rpcport"     8336
                 })
    where
      getSetting cp x dfl = case get cp "DEFAULT" x of
                              Left  _ -> dfl
                              Right x -> x

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
      Left  jerr -> Left $ "Unparseable response: " ++ (show (responseBody rsp))
      Right jrsp ->
        case decode (resValue jrsp) :: Maybe NmcDom of
          Nothing  -> Left $ "Unparseable value: " ++ (show (resValue jrsp))
          Just dom -> Right dom

-- NMC interface

queryNmc :: Manager -> Config -> String -> RRType -> String
         -> IO (Either String NmcDom)
queryNmc mgr cfg fqdn qtype qid = do
  case reverse (splitOn "." fqdn) of
    "bit":dn:xs -> do
      rsp <- runResourceT $
             httpLbs (qReq cfg (L.pack ("d/" ++ dn)) (L.pack qid)) mgr
      return $ qRsp rsp
    _           ->
      return $ Left "Only \".bit\" domain is supported"

-- Main entry

main = do

  cfg <- readConfig confFile

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

  print $ qReq cfg "d/nosuchdomain" "query-nmc"
  rsp <- runResourceT $ httpLbs (qReq cfg "d/nosuchdomain" "query-nmc") mgr
  print $ (statusCode . responseStatus) rsp
  putStrLn "===== complete response is:"
  print rsp
  let rbody = responseBody rsp
  putStrLn "===== response body is:"
  print rbody
  let result = parseJsonRpc rbody :: Either JsonRpcError NmcRes
  putStrLn "===== parsed response is:"
  print result
--  print $ parseJsonRpc (responseBody rsp)

  --forever $ getLine >>= (pdnsOut uri) . (pdnsParse ver)
