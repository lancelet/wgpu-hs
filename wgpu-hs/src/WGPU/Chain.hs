{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module WGPU.Chain where

import Control.Exception.Safe (Exception, throw)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT (ContT), evalContT)
import Foreign
  ( Ptr,
    Storable,
    alignment,
    allocaBytesAligned,
    castPtr,
    nullPtr,
    peek,
    poke,
    sizeOf,
  )
import WGPU.CStruct (CStruct)
import qualified WGPU.CStruct as CStruct
import WGPU.Types.ChainedStruct (ChainedStruct, STypeId)
import qualified WGPU.Types.ChainedStruct as ChainedStruct

empty :: Chain '[]
empty = ()

pattern (:&) :: e -> Chain es -> Chain (e : es)
pattern e :& es = (e, es)

infixr 7 :&

type Chain :: forall a. [a] -> a
type family Chain a = r | r -> a where
  Chain '[] = ()
  Chain (x : xs) = (x, Chain xs)

class HasSType struct where
  getSType :: struct -> STypeId

class HasChainPtr struct where
  peekChainPtr :: Ptr struct -> IO (Ptr ChainedStruct)
  pokeChainPtr :: Ptr struct -> Ptr ChainedStruct -> IO ()

class PokeChain es where
  withChain :: Chain es -> ((STypeId, Ptr (Chain es)) -> IO a) -> IO a

class PeekChain es where
  peekChain :: Ptr (Chain es) -> IO (Chain es)

instance PokeChain '[] where
  withChain () action = action (0, nullPtr)

instance
  (CStruct e, HasChainPtr e, HasSType e, PokeChain es) =>
  PokeChain (e : es)
  where
  withChain (e, es) action =
    evalContT $ do
      (tlId, tl) <- ContT $ withChain es
      hd <- ContT $ CStruct.withCStruct e
      cs <-
        ContT $
          withStorable $
            ChainedStruct.ChainedStruct
              { ChainedStruct.next = castPtr tl,
                ChainedStruct.sType = tlId
              }
      lift $ pokeChainPtr hd cs
      lift $ action (getSType e, castPtr hd)

withStorable :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
withStorable x action =
  allocaBytesAligned (sizeOf x) (alignment x) $ \ptr -> do
    poke ptr x
    action ptr

instance PeekChain '[] where
  peekChain _ = pure ()

instance
  (CStruct e, HasChainPtr e, HasSType e, PeekChain es) =>
  PeekChain (e : es)
  where
  peekChain ptr = do
    let hdPtr :: Ptr e
        hdPtr = castPtr ptr
    hd <- CStruct.peekCStruct @e hdPtr
    csPtr <- peekChainPtr hdPtr
    cs <- peek @ChainedStruct csPtr
    let requiredSTypeId :: STypeId
        requiredSTypeId = getSType (undefined :: e)
    unless (requiredSTypeId == ChainedStruct.sType cs) $
      throw
        ( MismatchedSTypeException
            (ChainedStruct.sType cs)
            requiredSTypeId
        )
    tl <- peekChain @es (castPtr (ChainedStruct.next cs))
    pure (hd, tl)

data MismatchedSTypeException = MismatchedSTypeException
  { sTypeId :: STypeId,
    expectedSTypeId :: STypeId
  }

instance Show MismatchedSTypeException where
  show e =
    "Mismatched sType id when reading a chained struct. Expected id "
      <> show (sTypeId e)
      <> ", but found id "
      <> show (expectedSTypeId e)

instance Exception MismatchedSTypeException
