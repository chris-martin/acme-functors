{-# LANGUAGE NoImplicitPrelude #-}

module Acme.Functors.Classes
    ( Applicative (..)
    , Monad       (..)
    , Eq          (..)
    , Functor     (..)
    , Monoid      (..)
    , Semigroup   (..)
    , Show        (..)
    ) where

import Control.Applicative ( Applicative (..) )
import Control.Monad       ( Monad       (..) )
import Data.Eq             ( Eq          (..) )
import Data.Functor        ( Functor     (..) )
import Data.Semigroup      ( Semigroup   (..) )
import Prelude             ( Show        (..) )

class Semigroup a => Monoid a where

    mempty :: a
