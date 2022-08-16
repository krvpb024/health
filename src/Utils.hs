{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Utils where

import Control.Monad.Reader
import Conf
import Servant
import GHC.Base

type ReaderHandler = ReaderT Env Handler

type PostRedirect (code :: Nat) loc
  = Verb 'POST code '[JSON] (Headers '[Header "Location" loc] NoContent)

redirect :: (Monad m, ToHttpApiData loc) => loc
                              -> m (Headers '[Header "Location" loc] NoContent)
redirect location = return (addHeader location NoContent)
