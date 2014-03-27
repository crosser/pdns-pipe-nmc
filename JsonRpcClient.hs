{-# LANGUAGE OverloadedStrings #-}

module JsonRpcClient
                ( JsonRpcVersion(JsonRpcV1, JsonRpcV2)
                , JsonRpcRequest(..)
                , JsonRpcNotification
                , JsonRpcError(..)
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
                                     , jrpcReqId      :: Value
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
instance FromJSON JsonRpcError where
  parseJSON (Object o) = JsonRpcError
                                <$> o .:  "code"
                                <*> o .:  "message"
                                <*> o .:? "data"
  parseJSON x = return $ JsonRpcError
                                (-32600)
                                "Unparseable error object"
                                (Just (toJSON x))

data JsonRpcResponse = JsonRpcResponse { jrpcRspResult :: Maybe Value
                                       , jrpcRspError  :: JsonRpcError
                                       , jrpcRspId     :: Value
                                       } deriving (Show)
instance FromJSON JsonRpcResponse where
  parseJSON (Object o) = JsonRpcResponse
                                <$> o .:? "result"
                                <*> o .:  "error"
                                <*> o .:  "id"
  parseJSON x = return $ JsonRpcResponse
                                Nothing
                                (JsonRpcError
                                        (-32700)
                                        "Unparseable response object"
                                        (Just (toJSON x))
                                )
                                (String "n/a")

parseJsonRpc :: (FromJSON a) => ByteString -> Either JsonRpcError a
parseJsonRpc s = case (decode s :: Maybe JsonRpcResponse) of
  Just (JsonRpcResponse result error id) ->
    case result of
      Just v -> case fromJSON v of
        Success a -> Right a
        Error s   -> Left $ JsonRpcError (-32900) "Unparseable result" (Just v)
      Nothing -> Left error
  Nothing -> Left $ JsonRpcError (-32800) "Unparseable response" (Just (toJSON s))
