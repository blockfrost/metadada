{-# LANGUAGE ScopedTypeVariables #-}

module Golden where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Golden (goldenVsFile)

import           Metadada.Types

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL

processMeta source dest = do
  contents <- BL.readFile source
  case Aeson.eitherDecode contents of
    Left e -> error e
    Right (meta :: Meta) ->
        BL.writeFile dest
      $ Aeson.encode meta

test_samples :: TestTree
test_samples = testGroup "golden"
  $ map mk [0..2]
  where
    mk n =
      let
        fp = "test/samples/sample"
        reg = (fp ++ show n ++ ".registry.json")
        act = (fp ++ show n ++ ".actual.json")
        our = (fp ++ show n ++ ".expected.json")
      in
        goldenVsFile
          ("golden sample of " ++ reg)
          act our (processMeta reg act)
