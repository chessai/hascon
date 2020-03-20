{-# language TemplateHaskell, TypeFamilies, DataKinds, UndecidableInstances, TypeApplications #-}

module Test where

import Hascon

data Foo = Foo1 | Foo2
mkHasCon ''Foo

data Bar = Bar1 String Int | Bar2 String | Bar3 Bool
mkHasCon ''Bar

foo :: String
foo = con @Foo @"Foo2"

bar :: String
bar = con @Bar @"Bar1"
