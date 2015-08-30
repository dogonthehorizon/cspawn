{-# LANGUAGE
    DeriveGeneric
    , OverloadedStrings
    #-}

module Lib where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.Yaml
import GHC.Generics
import Network.Nats
import System.Exit
import System.IO

import Debug.Trace

data ServerConfig = ServerConfig
    { natsConf :: NatsConf
    } deriving( Eq, Generic, Show )

instance FromJSON ServerConfig
instance ToJSON ServerConfig

data NatsConf = NatsConf
    { natsHost :: String
    , natsPort :: Int
    } deriving( Eq, Generic, Show )

instance FromJSON NatsConf
instance ToJSON NatsConf

-- | Read config from the yaml file at the given path.
-- @
--  readConfig "examples/config.yaml"
-- @
readConfig :: FilePath -> IO (Either String ServerConfig)
readConfig filePath = do
    eitherConf <- decodeFileEither filePath
    case eitherConf of
        Left ex    -> return . Left $
            prettyPrintParseException ex
        Right conf -> return $ Right conf

-- | Spin up the server.
launch :: FilePath -> IO ()
launch fp = do
    eitherConf <-  readConfig fp
    case eitherConf of
        Left err   -> putStrLn err
        Right conf -> launch' conf


launch' :: ServerConfig -> IO ()
launch' conf = do
    let addr  = buildAddr conf
    nats <- trace addr $ connect addr
    sid  <- subscribe nats "test" Nothing $ \_ _ msg _ -> putStrLn $ show msg
    loop nats
  where
    buildAddr :: ServerConfig -> String
    buildAddr conf =
        let nconf = natsConf conf
        in  "nats://" ++ (natsHost nconf) ++ ":" ++ (show $ natsPort nconf)

    loop :: Nats -> IO ()
    loop nats = do
        str <- getLine
        if str == "exit" then
            exitWith ExitSuccess
        else do
            -- Allow manual testing for now
            publish nats "test" $ BL.pack str
            loop nats

