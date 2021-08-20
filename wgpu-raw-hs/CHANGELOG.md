# Revision history for wgpu-raw-hs

## 0.2.0.0 -- 2021-08-22

- Microsoft Windows support.

## 0.1.0.3 -- 2021-08-20

- Removed slightly-broken `triangle` example. There is now a fully-working
  `triangle` example in the `wgpu-hs` package instead.

## 0.1.0.2 -- 2021-08-16

- Redo attempt at stubbing out `createSurface`.
- Improve README.

## 0.1.0.1 -- 2021-08-16

- Stub out Linux version of `createSurface` so that the package documentation
  will compile.

## 0.1.0.0 -- 2021-08-15

- Initial version. 
  - Mostly auto-generated bindings to the raw C API.
  - Surface creation for Metal on macOS.
  - Basic triangle example is kind-of working.
