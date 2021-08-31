{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    FromRaw (fromRaw),
    ToRawPtr (rawPtr),
    FromRawPtr (fromRawPtr),
    ReadableMemoryBuffer (withReadablePtr, readableMemoryBufferSize),

    -- * Types
    ByteSize (ByteSize, unByteSize),

    -- * Functions

    -- ** Internal
    toCSize,
    evalContT,
    allocaC,
    rawArrayPtr,
    showWithPtr,
    withCZeroingAfter,

    -- ** Lifted to MonadIO
    newEmptyMVar,
    takeMVar,
    putMVar,
    freeHaskellFunPtr,
    poke,
  )
where

import Control.Concurrent (MVar)
import qualified Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Cont (ContT (ContT), callCC, runContT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Storable as SVector
import Data.Word (Word32, Word8)
import Foreign
  ( FunPtr,
    Ptr,
    Storable,
    Word64,
    advancePtr,
    alignment,
    alloca,
    allocaArray,
    castPtr,
    nullPtr,
    peek,
    sizeOf,
  )
import qualified Foreign (fillBytes, freeHaskellFunPtr, poke)
import Foreign.C (CBool, CChar, CSize (CSize), peekCString, withCString)

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
  raw :: a -> ContT r IO b

-- | Represents a value of type @a@ that can be stored as type @(Ptr b)@ in the
-- 'ContT' monad.
--
-- Implementations of this type class should bracket resource management for
-- creating @('Ptr' b)@ around the continuation. In particular, the memory
-- allocated for @('Ptr' b)@ must be allocated before the continuation is
-- called, and freed afterward.
class ToRawPtr a b where
  rawPtr :: a -> ContT r IO (Ptr b)

-- | Represents a type @a@ that can be read from a raw value @b@.
class FromRaw b a | a -> b where
  fromRaw :: MonadIO m => b -> m a

-- | Represents a type @a@ that can be read from a raw pointer @b@.
class FromRawPtr b a where
  fromRawPtr :: MonadIO m => Ptr b -> m a

-------------------------------------------------------------------------------
-- Derived Functionality

-- | Return a pointer to an allocated array, populated with raw values from a
-- vector.
rawArrayPtr ::
  forall v r a b.
  (ToRaw a b, Storable b, Vector v a) =>
  -- | Vector of values to store in a C array.
  v a ->
  -- | Pointer to the array with raw values stored in it.
  ContT r IO (Ptr b)
rawArrayPtr xs = callCC $ \k -> do
  let pokeRaw :: a -> Ptr b -> ContT c IO ()
      pokeRaw value raw_ptr = raw value >>= liftIO . poke raw_ptr

      n :: Int
      n = Vector.length xs
  arrayPtr <- allocaArrayC n
  Vector.iforM_ xs $ \i x -> pokeRaw x (advancePtr arrayPtr i)
  r <- k arrayPtr
  zeroMemory arrayPtr (n * alignment (undefined :: b))
  pure r
{-# INLINEABLE rawArrayPtr #-}

-------------------------------------------------------------------------------
-- Instances

-- Allow every ToRaw instance to be a ToRawPtr instance.
instance {-# OVERLAPPABLE #-} (Storable b, ToRaw a b) => ToRawPtr a b where
  rawPtr x = raw x >>= withCZeroingAfter
  {-# INLINEABLE rawPtr #-}

instance {-# OVERLAPPABLE #-} (Storable b, FromRaw b a) => FromRawPtr b a where
  fromRawPtr ptr = (liftIO . peek) ptr >>= fromRaw
  {-# INLINEABLE fromRawPtr #-}

instance ToRaw Bool CBool where
  raw x = pure (if x then 1 else 0)
  {-# INLINE raw #-}

instance ToRaw Word32 Word32 where
  raw = pure

instance ToRawPtr Text CChar where
  rawPtr = withCStringC . Text.unpack
  {-# INLINEABLE rawPtr #-}

instance ToRawPtr ByteString Word8 where
  rawPtr = fmap castPtr . unsafeUseAsCStringC
  {-# INLINEABLE rawPtr #-}

instance FromRaw (Ptr CChar) Text where
  fromRaw ptr =
    if ptr == nullPtr
      then pure Text.empty
      else (liftIO . fmap Text.pack . peekCString) ptr
  {-# INLINEABLE fromRaw #-}

-------------------------------------------------------------------------------
-- Continuation helpers

allocaC :: Storable a => ContT r IO (Ptr a)
allocaC = ContT alloca
{-# INLINEABLE allocaC #-}

allocaArrayC :: Storable a => Int -> ContT r IO (Ptr a)
allocaArrayC sz = ContT (allocaArray sz)
{-# INLINEABLE allocaArrayC #-}

withCStringC :: String -> ContT r IO (Ptr CChar)
withCStringC str = ContT (withCString str)
{-# INLINEABLE withCStringC #-}

unsafeUseAsCStringC :: ByteString -> ContT r IO (Ptr CChar)
unsafeUseAsCStringC byteString = ContT (unsafeUseAsCString byteString)
{-# INLINEABLE unsafeUseAsCStringC #-}

withCZeroingAfter :: Storable a => a -> ContT r IO (Ptr a)
withCZeroingAfter x = callCC $ \k -> do
  ptr <- allocaC
  poke ptr x
  r <- k ptr
  zeroMemory ptr (sizeOf x)
  pure r
{-# INLINEABLE withCZeroingAfter #-}

-------------------------------------------------------------------------------
-- Memory actions lifted to MonadIO

newEmptyMVar :: MonadIO m => m (MVar a)
newEmptyMVar = liftIO Control.Concurrent.newEmptyMVar
{-# INLINEABLE newEmptyMVar #-}

takeMVar :: MonadIO m => MVar a -> m a
takeMVar = liftIO . Control.Concurrent.takeMVar
{-# INLINEABLE takeMVar #-}

putMVar :: MonadIO m => MVar a -> a -> m ()
putMVar mvar x = liftIO $ Control.Concurrent.putMVar mvar x
{-# INLINEABLE putMVar #-}

poke :: (MonadIO m, Storable a) => Ptr a -> a -> m ()
poke ptr value = liftIO (Foreign.poke ptr value)
{-# INLINEABLE poke #-}

freeHaskellFunPtr :: MonadIO m => FunPtr a -> m ()
freeHaskellFunPtr = liftIO . Foreign.freeHaskellFunPtr
{-# INLINEABLE freeHaskellFunPtr #-}

fillBytes :: MonadIO m => Ptr a -> Word8 -> Int -> m ()
fillBytes ptr x sz = liftIO (Foreign.fillBytes ptr x sz)
{-# INLINEABLE fillBytes #-}

zeroMemory :: MonadIO m => Ptr a -> Int -> m ()
zeroMemory ptr = fillBytes ptr 0x00
{-# INLINEABLE zeroMemory #-}

-------------------------------------------------------------------------------

evalContT :: Monad m => ContT a m a -> m a
evalContT cont = runContT cont pure

-------------------------------------------------------------------------------

-- | Size, in number of bytes.
newtype ByteSize = ByteSize {unByteSize :: Word64}
  deriving (Eq, Ord, Show, Enum, Real, Integral, Num)

instance ToRaw ByteSize CSize where
  raw = pure . toCSize

toCSize :: ByteSize -> CSize
toCSize = CSize . unByteSize

-------------------------------------------------------------------------------

-- | Region of memory that is read-only for WGPU.
--
-- A 'ReadableMemoryBuffer' represents a contiguous region of memory that WGPU
-- may read from, but not write to. It has a pointer to the start of the region,
-- and a size in bytes.
--
-- When the caller of WGPU supplies a 'ReadableMemoryBuffer', it commits to
-- keeping the buffer alive for the period of the call to `withReadablePtr`.
-- WGPU commits to not mutating the data.
--
-- If it is necessary to refer to slices within a buffer, it is up to the type
-- @a@ to store those offsets and account for them in the two functions required
-- by the API. (For example, 'SVector.Vector' does this.)
class ReadableMemoryBuffer a where
  -- | Perform an action with the memory buffer.
  withReadablePtr :: a -> (Ptr () -> IO b) -> IO b

  -- | The size of the buffer, in number of bytes.
  readableMemoryBufferSize :: a -> ByteSize

instance {-# OVERLAPPABLE #-} Storable a => ReadableMemoryBuffer a where
  withReadablePtr x action =
    alloca $ \ptr -> do
      poke ptr x
      result <- action (castPtr ptr)
      zeroMemory ptr (sizeOf (undefined :: a))
      pure result
  {-# INLINEABLE withReadablePtr #-}
  readableMemoryBufferSize x = ByteSize (fromIntegral (sizeOf x))
  {-# INLINEABLE readableMemoryBufferSize #-}

instance
  {-# OVERLAPPABLE #-}
  Storable a =>
  ReadableMemoryBuffer (SVector.Vector a)
  where
  withReadablePtr vec action = SVector.unsafeWith vec (action . castPtr)
  {-# INLINEABLE withReadablePtr #-}
  readableMemoryBufferSize vec =
    ByteSize . fromIntegral $ sizeOf (undefined :: a) * SVector.length vec
  {-# INLINEABLE readableMemoryBufferSize #-}

-------------------------------------------------------------------------------

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
{-# INLINEABLE showWithPtr #-}
