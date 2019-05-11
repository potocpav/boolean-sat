{- |
Module      :  $Header$
Description :  Monad generating fresh integers

A simple 'State' monad wrapper, which is specialized to sequentially return
unique integers using the 'fresh' function.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Supply where

import Control.Monad
import Control.Monad.State


-- | Supply monad
newtype Supply a = Supply { unSupply :: State Int a } deriving (Functor, Applicative, Monad)


-- | Run the monad using a starting value
runSupply :: Supply a -> Int -> (a, Int)
runSupply (Supply st) i = runState st i


-- | Get a fresh integer and advance the internal counter
fresh :: Supply Int
fresh = Supply $ do
    n <- get
    put (n + 1)
    pure n


-- | Get the same integer a 'fresh' call would get, but without advancing the
-- counter
peek :: Supply Int
peek = Supply get
