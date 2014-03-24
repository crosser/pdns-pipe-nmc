{-# LANGUAGE OverloadedStrings #-}

module JsonRpc  ( JsonRpcVersion(JsonRpcV1, JsonRpcV2)
                , JsonRpcRequest
                , JsonRpcNotification
                , JsonRpcError
                , JsonRpcResponse
                , parseJsonRpc
                ) where

import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Either
import Data.Aeson

data JsonRpcVersion = JsonRpcV1 | JsonRpcV2
        deriving (Show)

data JsonRpcRequest = JsonRpcRequest { jrpcVersion    :: JsonRpcVersion
                                     , jrpcReqMethod  :: ByteString
                                     , jrpcReqParams  :: [ByteString]
                                     , jrpcReqId      :: ByteString
                                     } deriving (Show)
instance ToJSON JsonRpcRequest where
  toJSON (JsonRpcRequest version method params id) =
    let l = [ "method" .= method, "params" .= params, "id" .= id ]
    in case version of
      JsonRpcV1 -> object l
      JsonRpcV2 -> object $ ("jsonrpc" .= toJSON ("2.0" :: ByteString)):l

data JsonRpcNotification = JsonRpcNotification
                                     { jrpcNtfVersion :: JsonRpcVersion
                                     , jrpcNtfMethod  :: ByteString
                                     , jrpcNtfParams  :: [ByteString]
                                     } deriving (Show)
instance ToJSON JsonRpcNotification where
  toJSON (JsonRpcNotification version method params) =
    let l = [ "method" .= method, "params" .= params ]
    in case version of
      JsonRpcV1 -> object l
      JsonRpcV2 -> object $ ("jsonrpc" .= toJSON ("2.0" :: ByteString)):l

data JsonRpcError = JsonRpcError { jrpcErrCode    :: Int
                                 , jrpcErrMessage :: ByteString
                                 , jrpcErrData    :: Maybe Value
                                 } deriving (Show)

data JsonRpcResponse = JsonRpcResponse { jrpcRspResult :: Maybe Value
                                       , jrpcRspError  :: JsonRpcError
                                       , jrpcRspId     :: ByteString
                                       } deriving (Show)

parseJsonRpc :: ByteString -> Either JsonRpcError JsonRpcResponse
parseJsonRpc _ = Left $ JsonRpcError (-1) "someerror" Nothing
