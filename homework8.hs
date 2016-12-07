-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Home8 where
import Data.Monoid
import Sized
import Data.Char
import qualified Data.Map as Map



main = print $  scoreLine "yay " +++ scoreLine "haskell!"