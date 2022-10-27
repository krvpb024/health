{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.TemplateHandler where

import Data.HashMap.Strict as HS
import Data.Hashable
import System.Exit
import System.IO
import System.IO.Error
import Text.Ginger
import Text.Ginger.Html
import Data.ByteString.Lazy as BL
import Data.Text.Lazy.Encoding as TLE
import Data.Text.Lazy as TL
import System.Directory
import Servant
import Network.HTTP.Media ((//), (/:))
import Control.Monad.IO.Class
import Control.Exception

scopeLookup :: (Hashable k, Eq k, ToGVal m b) => k
                                              -> HS.HashMap k b
                                              -> GVal m
scopeLookup key context = toGVal $ HS.lookup key context

loadFileMay :: FilePath -> IO (Maybe String)
loadFileMay fileName = do
  dir <- getCurrentDirectory
  fileContent <- tryIOError (loadFile $ dir <> "/templates-dist" <> fileName)
  case fileContent of
    Right contents -> pure $ Just contents
    Left  ioErr    -> throw ioErr

  where loadFile :: FilePath -> IO String
        loadFile filePath = openFile filePath ReadMode >>= System.IO.hGetContents

render :: Template SourcePos
       -> HS.HashMap VarName BL.ByteString
       -> BL.ByteString
render template contextMap =
  TLE.encodeUtf8 $ TL.fromStrict $
  htmlSource $ runGinger context template

  where contextLookup = flip scopeLookup contextMap
        context = makeContextHtml contextLookup

data HTML = HTML

newtype RawHtml = RawHtml { unRaw :: BL.ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

htmlHandler :: MonadIO m => HS.HashMap VarName BL.ByteString
                         -> String
                         -> m RawHtml
htmlHandler context template = do
  htmlTemplate <- liftIO $ parseGingerFile loadFileMay template
  case htmlTemplate of
    Left  parseErr -> throw parseErr
    Right tem      -> return $ RawHtml $ render tem context