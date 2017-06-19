{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.FastRecord.Rec where

import Data.FastRecord.Schema
import Data.Kind
import GHC.Exts
import GHC.TypeLits

data Rec :: [(Symbol, Type)] -> Type where
  Rec :: SmallArray# Any -> Rec ts

nullRec# :: KnownNat (SchemaSize ts) => Proxy# ts -> Rec ts
nullRec# ts =
  case newSmallArray# (schemaSize# ts) undefined realWorld# of
    (# s1, buf #) ->
      case unsafeFreezeSmallArray# buf s1 of
        (# _, arr #) -> Rec arr

{-# INLINE getField# #-}
getField# ::
     KnownNat (FieldIndex ts sym t)
  => Proxy# ts
  -> Proxy# sym
  -> Proxy# t
  -> Rec ts
  -> t
getField# ts sym t (Rec arr) =
  case indexSmallArray# arr (fieldIndex# ts sym t) of
    (# v #) -> unsafeCoerce# v

{-# INLINE setField# #-}
setField# ::
     (KnownNat (SchemaSize ts), KnownNat (FieldIndex ts sym t))
  => Proxy# ts
  -> Proxy# sym
  -> Proxy# t
  -> Rec ts
  -> t
  -> Rec ts
setField# ts sym t (Rec arr) v =
  case thawSmallArray# arr 0# (schemaSize# ts) realWorld# of
    (# s1, buf #) ->
      case unsafeFreezeSmallArray#
             buf
             (writeSmallArray# buf (fieldIndex# ts sym t) (unsafeCoerce# v) s1) of
        (# _, arr' #) -> Rec arr'
