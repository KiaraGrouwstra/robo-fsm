{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude
import Control.Monad.Indexed
import Control.Monad.Indexed.State
import Control.Monad.Indexed.Trans
import Control.Monad.IO.Class
import qualified Language.Haskell.Rebindable as Use
-- import Data.Default
-- import Lib

main :: IO ()
main = someFunc

foo :: IxStateT IO String Int ()
foo = let Use.IxMonad{..} = def in do
  ilift . liftIO . print =<< iget

  imodify (length)
  ilift . liftIO . print =<< iget
