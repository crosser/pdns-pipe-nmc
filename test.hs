{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack, readFile)
import System.IO.Error
import Control.Exception

import NmcDom

queryOp :: ByteString -> IO (Either String ByteString)
queryOp key = catch (readFile ("data/" ++ (unpack key)) >>= return . Right)
                    (\e -> return (Left (show (e :: IOException))))

main = do
        d <- queryDom queryOp "root"
        putStrLn $ show d

