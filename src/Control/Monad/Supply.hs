{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Supply where

import Control.Monad
import Control.Monad.State


newtype Supply a = Supply { unSupply :: State Int a } deriving (Functor, Applicative, Monad)


runSupply :: Supply a -> Int -> (a, Int)
runSupply (Supply st) i = runState st i


fresh :: Supply Int
fresh = Supply $ do
    n <- get
    put (n + 1)
    pure n


peek :: Supply Int
peek = Supply get
