{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : WGPU.Internal.Memory
-- Description : Managing memory.
--
-- This module contains type classes used to manage marshalling of objects into
-- memory before calling C functions.
--
-- = Motivation
--
-- In many locations in the API, we have:
--
-- A type (example only) which contains a nice Haskell representation of
-- some data:
--
-- @
-- data ApiType = ApiType { things :: Vector Thing }
-- @
--
-- and a raw type which is required for a C function:
--
-- @
-- data WGPUApiType = WGPUApiType
--   { thingsCount :: 'Word8',            -- this is an array length
--     things      :: 'Ptr' WGPUApiThing  -- this is a pointer to an array
--   }
-- @
--
-- This type class constraint represents the ability to encode @ApiType@ as
-- @WGPUApiType@, performing any necessary memory allocation and freeing:
--
-- @
-- 'ToRaw' ApiType WGPUApiType
-- @
--
-- 'ToRaw' uses the 'ContT' monad so that bracketing of the memory resources
-- can be performed around some continuation that uses the memory.
--
-- In the example above, we could write a 'ToRaw' instance as follows:
--
-- @
-- instance 'ToRaw' ApiType WGPUApiType where
--   'raw' ApiType{..} = do
--     names_ptr <- 'rawArrayPtr' names
--     'pure' $ WGPUApiType
--       { namesCount = fromIntegral . length $ names,
--         names      = names_ptr
--       }
-- @
--
-- The 'ToRawPtr' type class represents similar functionality, except that it
-- creates a pointer to a value. Thus it does both raw conversion and storing
-- the raw value in allocated memory. It exists as a separate type class so
-- that library types (eg. 'Text' and 'ByteString') can be marshalled into
-- pointers more easily.
module WGPU.Internal.Memory
  ( -- * Classes
    ToRaw (raw),
    ToRawPtr (rawPtr),

    -- * Functions
    rawArrayPtr,
    showWithPtr,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (ContT), evalContT)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector
import Data.Word (Word8)
import Foreign
  ( Ptr,
    Storable,
    advancePtr,
    allocaArray,
    castPtr,
    fillBytes,
    poke,
    sizeOf,
    with,
  )
import Foreign.C (CBool, CChar, withCString)

-------------------------------------------------------------------------------
-- Type Classes

-- | Represents a value of type @a@ that can be stored as type @b@ in the
-- 'ContT' monad.
--
-- Implementations of this type class should bracket any resource management for
-- creating the @b@ value around the continuation. For example. memory to hold
-- elements of @b@ should be allocated and freed in a bracketed fashion.
class ToRaw a b | a -> b where
  -- | Convert a value to a raw representation, bracketing any resource
  -- management.
  raw :: a -> ContT c IO b

-- | Represents a value of type @a@ that can be stored as type @(Ptr b)@ in the
-- 'ContT' monad.
--
-- Implementations of this type class should bracket resource management for
-- creating @('Ptr' b)@ around the continuation. In particular, the memory
-- allocated for @('Ptr' b)@ must be allocated before the continuation is
-- called, and freed afterward.
class ToRawPtr a b where
  rawPtr :: a -> ContT c IO (Ptr b)

-------------------------------------------------------------------------------
-- Derived Functionality

-- | Return a pointer to an allocated array, populated with raw values from a
-- vector.
rawArrayPtr ::
  forall v a b c.
  (ToRaw a b, Storable b, Vector v a) =>
  -- | Vector of values to store in a C array.
  v a ->
  -- | Pointer to the array with raw values stored in it.
  ContT c IO (Ptr b)
rawArrayPtr xs =
  ContT $ \action -> do
    let n :: Int
        n = Vector.length xs
    allocaArray n $ \arrayPtr ->
      evalContT $ do
        forM_
          (zip (Vector.toList xs) [0 ..])
          (\(x, i) -> pokeRaw x (advancePtr arrayPtr i))
        liftIO (action arrayPtr)
  where
    pokeRaw :: a -> Ptr b -> ContT c IO ()
    pokeRaw value raw_ptr = raw value >>= liftIO . poke raw_ptr

-------------------------------------------------------------------------------
-- Instances

-- Allow every ToRaw instance to be a ToRawPtr instance.
instance {-# OVERLAPPABLE #-} (Storable b, ToRaw a b) => ToRawPtr a b where
  rawPtr x = do
    rawX <- raw x
    ContT $ zeroingWith rawX

instance ToRaw Bool CBool where raw x = pure (if x then 1 else 0)

instance ToRawPtr Text CChar where rawPtr = ContT . withCString . Text.unpack

instance ToRawPtr ByteString Word8 where
  rawPtr byteString =
    ContT $ \action -> unsafeUseAsCString byteString (action . castPtr)

-------------------------------------------------------------------------------
-- Utils

-- | Like 'with', but zeroes memory after the action has been performed.
--
-- Allocates memory for a value of type @a@ and fills the memory with the
-- 'Foreign.Storable' representation of the @a@ value.
zeroingWith ::
  Storable a =>
  -- | Value to use.
  a ->
  -- | Action to perform with a pointer to the value.
  (Ptr a -> IO b) ->
  -- | Result of running the action.
  IO b
zeroingWith value action =
  with value $ \value_ptr -> do
    result <- action value_ptr
    fillBytes value_ptr 0x00 (sizeOf value)
    pure result

-- | Formatter for 'Show' instances for opaque pointers.
--
-- Displays a name and a corresponding opaque pointer.
showWithPtr ::
  -- | Name of the type.
  String ->
  -- | Opaque pointer that the type contains.
  Ptr a ->
  -- | Final show string.
  String
showWithPtr name ptr = "<" <> name <> ":" <> show ptr <> ">"
