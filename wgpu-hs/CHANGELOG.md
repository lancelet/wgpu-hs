# Revision history for wgpu-hs

## 0.3.0.0 -- 2021-08-30

- Add Classy interface - supply parameters from `ReaderT`.
- Add SDL surfaces.
- Add `BoneYard` package for helpful application skeleton code.
- Switch to using `MonadIO` instead of plain `IO` when possible.
- Supply bracketing functions for `withXXX`.
- Add extra `{-# INLINABLE #-}` pragmas.
- Add ability to query Adapter properties.

## 0.2.0.1 -- 2021-08-24

- Added Linux support for surfaces and DLL loading.
- Updated `triangle` example to reallocate SwapChain correctly on resize.

## 0.2.0.0 -- 2021-08-22

- Added MS Windows support for surfaces and DLL loading.
- Added `withPlatformInstance`: select dynamic library name based on platform.

## 0.1.0.0 -- 2021-08-20

- Initial (incomplete) API bindings started.
- Working `triangle` example.
