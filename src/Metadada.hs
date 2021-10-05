{-# LANGUAGE TypeApplications #-}
module Metadada where

import Control.Monad (forM_)
import Data.ByteString.Lazy (ByteString)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified System.Directory
import qualified System.Environment
import           System.FilePath ((</>))

import Metadada.Types (Meta(..))

convertMeta :: ByteString -> ByteString
convertMeta = Aeson.encode . Aeson.decode @Meta

-- | stdin -> convertMeta -> stdout
app :: IO ()
app = BL.interact convertMeta

-- | Convert directory of metadata jsons
dir :: IO ()
dir = do
  args <- System.Environment.getArgs
  case args of
    [inDir, outDir] -> do

      System.Directory.createDirectoryIfMissing True outDir

      files <- System.Directory.listDirectory inDir

      forM_ files $ \f -> do
        fc <- BL.readFile (inDir </> f)
        case Aeson.eitherDecode @Meta fc of
          Left e -> error $ "Unable to decode: " ++ show e
          Right meta -> BL.writeFile (outDir </> f) (Aeson.encode meta)

    _otherwise -> error "Usage: metadada-convert-dir inDir outDir"
