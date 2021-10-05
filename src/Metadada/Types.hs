{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Metadada.Types (Meta(..)) where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson

-- Metadata entry
-- https://github.com/cardano-foundation/cardano-token-registry#semantic-content-of-registry-entries
data Meta = Meta {
    subject     :: Text -- ^ The base16-encoded policyId + base16-encoded assetName
  , name        :: Text -- ^ A human-readable name for the subject, suitable for use in an interface
  , description :: Text -- ^ A human-readable description for the subject, suitable for use in an interface
  , policy      :: Maybe Text -- ^ The base16-encoded CBOR representation of the monetary policy script, used to verify ownership. Optional in the case of Plutus scripts as verification is handled elsewhere.
  , ticker      :: Maybe Text  -- ^ A human-readable ticker name for the subject, suitable for use in an interface
  , url         :: Maybe Text  -- ^ A HTTPS URL (web page relating to the token)
  , logo        :: Maybe Text  -- ^ A PNG image file as a byte string
  , decimals    :: Maybe Int  -- ^ How many decimals to the token
  }
  deriving (Eq, Ord, Show, Generic)

-- subject & policy are simple k:v
-- rest has value
-- ToJSON is our format (returned by API)
-- FromJSON uses token-registry format

instance ToJSON Meta where
  toEncoding Meta{..} = Aeson.pairs
    (   "name"        .= name
    <> "description" .= description
    <> "ticker"      .= ticker
    <> "url"         .= url
    <> "logo"        .= logo
    <> "decimals"    .= decimals
    )

instance FromJSON Meta where
  parseJSON = Aeson.withObject "Meta" $ \obj ->
    Meta
    <$> obj .: "subject"
    <*> parseValue obj "name"
    <*> parseValue obj "description"
    <*> obj .: "policy"
    <*> parseValueMaybe obj "ticker"
    <*> parseValueMaybe obj "url"
    <*> parseValueMaybe obj "logo"
    <*> parseValueMaybe obj "decimals"
    where
      parseValue obj label = do
        obj' <- obj .: label
        obj' .: "value"
      parseValueMaybe obj label = do
        obj' <- obj .:? label
        case obj' of
          Nothing -> pure Nothing
          Just x -> x .: "value"
