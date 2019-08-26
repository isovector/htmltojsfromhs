{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Lib where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Text.HTML.TagSoup.Tree

makeBaseFunctor ''TagTree



someFunc :: IO ()
someFunc = putStrLn "someFunc"
