module JsonRpc  ( JsonRpcRequest(..)
                , JsonRpcNotification(..)
                , JsonRpcResponse(..)
                ) where

import Data.ByteString (ByteString)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson

data JsonRpcRequest = JsonRpcRequest { jrpcReqMethod :: ByteString
                                     , jrpcReqParams :: [ByteString]
                                     , jrpcReqId     :: ByteString
                                     } deriving (Show)

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


