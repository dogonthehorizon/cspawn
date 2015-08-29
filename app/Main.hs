{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Network.Nats
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    nats <- connect "nats://172.17.42.1:4222"
    sid <- subscribe nats "test" Nothing $ \_ _ msg _ -> putStrLn $ show msg
    publish nats "test" "Tester"


