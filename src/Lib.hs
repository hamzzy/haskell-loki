module Lib
    ( errorLog,infoLog,warnLog
    ) where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (throwIO)
import Data.Aeson (ToJSON,FromJSON, encode, object)
import Control.Lens ((.~), (^.), (^?),(?~))
import Data.Aeson
import Data.Aeson.Types (Value(..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (Maybe, fromMaybe)
import Data.Text
import Data.Aeson.Lens 
import Data.Time.Clock (getCurrentTime)
import GHC.Generics (Generic)
import Network.Wreq
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Types (Pair)
import Relude hiding (ByteString,encodeUtf8, ask)
import Data.Text.Encoding
import Effectful.Reader.Static (ask)
import Data.Aeson (encode, object, (.=))

data LogData = LogData
  { streams :: [Stream]
  }
  deriving (Show, Generic)

data Stream = Stream
  { stream :: StreamInfo
  , values :: [[String]]
  }
  deriving (Show, Generic)

data StreamInfo = StreamInfo
  { label :: String
  }
  deriving (Show, Generic)

instance ToJSON LogData
instance FromJSON LogData
instance ToJSON Stream
instance FromJSON Stream
instance ToJSON StreamInfo
instance FromJSON StreamInfo

callApi :: String -> String -> Env -> IO ()
callApi level message tsEnv = do
  now <- liftIO getCurrentTimeInNanoseconds
  let nowText = pack . show $ now
      messageText = pack message
      logData = LogData
        [ Stream
            (StreamInfo (level))
            [ [""]
            , [""]
            ]
        ]
      logDataJson = BS.unpack $ encode logData
      opts = defaults
        & auth ?~ basicAuth (encodeUtf8 $ tsEnv.lokiUsername) (encodeUtf8 $ tsEnv.lokiPassword)
        & header "Content-Type" .~ ["application/json"]
  resp <- postWith opts (toString $ tsEnv.lokiUrl) (BS.pack logDataJson)
  print (resp ^. responseStatus)


customLevel :: String ->  String -> Env -> IO ()
customLevel level message tsEnv = callApi level message  tsEnv

errorLog :: String -> Env -> IO ()
errorLog message tsEnv = customLevel "error" message tsEnv

infoLog :: String -> Env -> IO ()
infoLog message tsEnv = customLevel "info"  message tsEnv

warnLog :: String -> Env -> IO ()
warnLog message tsEnv = customLevel "warn"  message tsEnv

debugLog :: String -> Env -> IO ()
debugLog message tsEnv = customLevel "debug" message tsEnv
