{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Data.FastRecord.Example where

import Data.FastRecord.BulkSet
import Data.FastRecord.Field
import Data.FastRecord.Rec
import Data.Monoid
import Data.Proxy
import Lens.Micro

type Schema = '[ '( "foo", Bool), '( "bar", String)]

bs :: BulkSet Schema
bs = Proxy @"foo" ~= True <> Proxy @"bar" ~= "233"

r0 :: Rec Schema
r0 = newRec bs

s0 :: String
s0 = r0 ^. field #bar

r1 :: Rec Schema
r1 = r0 & field #bar .~ "23333"

s1 :: String
s1 = r1 ^. field #bar
