{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FastRecord.Field where

import Data.FastRecord.Rec
import Data.FastRecord.Schema
import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits
import Lens.Micro

newtype Field ts t = Field
  { field :: Lens' (Rec ts) t
  }

instance (KnownNat (SchemaSize ts), KnownNat (FieldIndex ts sym t)) =>
         IsLabel sym (Field ts t) where
  fromLabel _ =
    Field
      (lens
         (getField# proxy# (proxy# :: Proxy# sym) proxy#)
         (setField# proxy# (proxy# :: Proxy# sym) proxy#))
