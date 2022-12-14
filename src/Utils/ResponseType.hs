{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.ResponseType where

import qualified Data.ByteString.Lazy.UTF8 as BLU
import Servant.API.ContentTypes
import Network.HTTP.Media ((//), (/:))


data HTML = HTML

newtype RawHtml = RawHtml { unRawHtml :: BLU.ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRawHtml

data CSV = CSV

newtype RawCsv = RawCsv { unRawCsv :: BLU.ByteString }


instance Accept CSV where
  contentType _ = "text" // "csv" /: ("charset", "utf-8")

instance MimeRender CSV RawCsv where
  mimeRender _ = unRawCsv

instance MimeRender CSV RawHtml where
  mimeRender _ = unRawHtml
