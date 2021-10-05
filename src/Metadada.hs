{-# LANGUAGE TypeApplications #-}
module Metadada where

import Data.ByteString.Lazy (ByteString)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL

import Metadada.Types (Meta(..))

convertMeta :: ByteString -> ByteString
convertMeta = Aeson.encode . Aeson.decode @Meta

-- | stdin -> convertMeta -> stdout
app :: IO ()
app = BL.interact convertMeta
