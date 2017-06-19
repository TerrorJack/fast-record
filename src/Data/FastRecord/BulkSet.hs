{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.FastRecord.BulkSet where

import Data.FastRecord.Rec
import Data.FastRecord.Schema
import Data.Kind
import Data.Proxy
import GHC.Exts
import GHC.TypeLits

newtype BulkSet (ts :: [(Symbol, Type)]) = BulkSet
  { bulkSetBuf# :: SmallMutableArray# RealWorld Any -> State# RealWorld -> State# RealWorld
  }

instance Monoid (BulkSet ts) where
  mempty = BulkSet (\_ s -> s)
  mappend (BulkSet f0) (BulkSet f1) = BulkSet (\buf s0 -> f1 buf (f0 buf s0))

bulkSetField# ::
     KnownNat (FieldIndex ts sym t)
  => Proxy# ts
  -> Proxy# sym
  -> Proxy# t
  -> t
  -> BulkSet ts
bulkSetField# ts sym t v =
  BulkSet
    (\buf -> writeSmallArray# buf (fieldIndex# ts sym t) (unsafeCoerce# v))

bulkSetProxy# :: BulkSet ts -> Proxy# ts
bulkSetProxy# _ = proxy#

bulkSet :: KnownNat (SchemaSize ts) => Rec ts -> BulkSet ts -> Rec ts
bulkSet (Rec arr) bs@(BulkSet f) =
  case thawSmallArray# arr 0# (schemaSize# (bulkSetProxy# bs)) realWorld# of
    (# s1, buf #) ->
      case unsafeFreezeSmallArray# buf (f buf s1) of
        (# _, arr' #) -> Rec arr'

newRec :: KnownNat (SchemaSize ts) => BulkSet ts -> Rec ts
newRec bs@(BulkSet f) =
  case newSmallArray# (schemaSize# (bulkSetProxy# bs)) undefined realWorld# of
    (# s1, buf #) ->
      case unsafeFreezeSmallArray# buf (f buf s1) of
        (# _, arr #) -> Rec arr

(~=) :: KnownNat (FieldIndex ts sym t) => Proxy sym -> t -> BulkSet ts
(~=) p = bulkSetField# proxy# (f p) proxy#
  where
    f :: Proxy sym -> Proxy# sym
    f _ = proxy#

infix 7 ~=
