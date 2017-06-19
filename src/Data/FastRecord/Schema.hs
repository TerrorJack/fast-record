{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Data.FastRecord.Schema where

import Data.Kind
import GHC.Exts
import GHC.Integer.GMP.Internals
import GHC.TypeLits

{-# INLINE natInt# #-}
natInt# :: KnownNat n => Proxy# n -> Int#
natInt# n =
  case natVal' n of
    S# x -> x

type family SchemaSize (ts :: [(Symbol, Type)]) :: Nat where
  SchemaSize '[] = 0
  SchemaSize (_ ': ts) = 1 + SchemaSize ts

schemaSizeProxy# ::
     KnownNat (SchemaSize ts) => Proxy# ts -> Proxy# (SchemaSize ts)
schemaSizeProxy# _ = proxy#

{-# INLINE schemaSize# #-}
schemaSize# :: KnownNat (SchemaSize ts) => Proxy# ts -> Int#
schemaSize# ts = natInt# (schemaSizeProxy# ts)

type family FieldIndex (ts :: [(Symbol, Type)]) (sym :: Symbol) (t :: Type) :: Nat where
  FieldIndex ('( sym, t) ': _) sym t = 0
  FieldIndex (_ ': ts) sym t = 1 + FieldIndex ts sym t

fieldIndexProxy# ::
     KnownNat (FieldIndex ts sym t)
  => Proxy# ts
  -> Proxy# sym
  -> Proxy# t
  -> Proxy# (FieldIndex ts sym t)
fieldIndexProxy# _ _ _ = proxy#

{-# INLINE fieldIndex# #-}
fieldIndex# ::
     KnownNat (FieldIndex ts sym t)
  => Proxy# ts
  -> Proxy# sym
  -> Proxy# t
  -> Int#
fieldIndex# ts sym t = natInt# (fieldIndexProxy# ts sym t)
