{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Route.Static where

import Servant

type StaticAPI = "static" :> Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

staticServer :: Server StaticAPI
staticServer = serveDirectoryWebApp "templates-dist"
