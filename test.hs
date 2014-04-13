{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (readFile)
import System.IO.Error
import Control.Exception

import NmcDom

queryOp :: String -> IO (Either String ByteString)
queryOp key = catch (readFile key >>= return . Right)
                    (\e -> return (Left (show (e :: IOException))))

main = do
        descendNmcDom queryOp []               (seedNmcDom "root") >>= print
        descendNmcDom queryOp ["_tcp","_smtp"] (seedNmcDom "root") >>= print

