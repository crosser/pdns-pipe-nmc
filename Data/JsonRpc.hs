{-# LANGUAGE OverloadedStrings #-}

module JsonRpc  ( JsonRpcVersion(JsonRpcV1 ,JsonRpcV2)
                , JsonRpcRequest
                , JsonRpcNotification
                , JsonRpcResponse
                ) where

import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>), (<*>), empty)
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
    let l = [ "method" .= method , "params" .= params , "id" .= id ]
    in case version of
      JsonRpcV1 -> object l
      JsonRpcV2 -> object $ ("jsonrpc" .= toJSON ("2.0" :: ByteString)):l
    
data JsonRpcNotification = JsonRpcNotification { jrpcNtfMethod :: ByteString
                                               , jrpcNtfParams :: [ByteString]
                                               } deriving (Show)

data JsonRpcError = JsonRpcError { jrpcErrCode    :: Int
                                 , jrpcErrMessage :: ByteString
                                 , jrpcErrData    :: Maybe Value
                                 } deriving (Show)

data JsonRpcResponse = JsonRpcResponse { jrpcRspResult :: Maybe Value
                                       , jrpcRspError  :: JsonRpcError
                                       , jrpcRspId     :: ByteString
                                       } deriving (Show)
