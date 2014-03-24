{-# LANGUAGE OverloadedStrings #-}

module JsonRpc  ( JsonRpcRequestV1
                , JsonRpcRequestV2
                , JsonRpcNotification
                , JsonRpcResponse
                ) where

import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson

data JsonRpcRequestV1 = JsonRpcRequestV1 { jrpcReqMethod1  :: ByteString
                                         , jrpcReqParams1  :: [ByteString]
                                         , jrpcReqId1      :: ByteString
                                         } deriving (Show)
instance ToJSON JsonRpcRequestV1 where
  toJSON (JsonRpcRequestV1 method params id) =
    object [ "method"  .= method
           , "params"  .= params
           , "id"      .= id ]
    
data JsonRpcRequestV2 = JsonRpcRequestV2 { jrpcReqMethod2  :: ByteString
                                         , jrpcReqParams2  :: [ByteString]
                                         , jrpcReqId2      :: ByteString
                                         } deriving (Show)
instance ToJSON JsonRpcRequestV2 where
  toJSON (JsonRpcRequestV2 jrpcReqMethod2 jrpcReqParams2 jrpcReqId2) =
    object [ "jsonrpc" .= toJSON ("2.0" :: ByteString)
           , "method"  .= jrpcReqMethod2
           , "params"  .= jrpcReqParams2
           , "id"      .= jrpcReqId2 ]

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
