# Haskell Bindings for WebGPU Native

[![Hackage][hackage-shield]][hackage]
[![CI Build][github-ci-shield]][github-ci]
[![License BSD-3-Clause][license-shield]][license]

[hackage]: http://hackage.haskell.org/package/wgpu-hs
[hackage-shield]: https://img.shields.io/hackage/v/wgpu-hs.svg?logo=haskell
[github-ci]: https://github.com/lancelet/wgpu-hs/actions
[github-ci-shield]: https://github.com/lancelet/wgpu-hs/actions/workflows/ci.yml/badge.svg
[license]: https://github.com/lancelet/wgpu-hs/blob/master/LICENSE
[license-shield]: https://img.shields.io/badge/license-BSD--3--Clause-green.svg

This repository contains Haskell bindings for
[wgpu-native](https://github.com/gfx-rs/wgpu-native).
These bindings are in an early stage of development and are not yet stable.

Currently, only macOS is supported by these Haskell bindings. Adding other
platforms should be relatively trivial but requires testing.

## Building and Running

You will need a working Rust toolchain.

To build and run an example:

  1. Clone the repository and make sure that all git submodules are checked
     out:
     
     ```
     git clone https://github.com/lancelet/wgpu-hs.git
     cd wgpu-hs
     git submodule update --init --recursive
     ```
       
  2. Build the Rust libraries. The `WGPU_NATIVE_VERSION` environment variable
     is optional, but if it is supplied, it bakes the specified version number
     into the dynamic library binary.
  
     ```
     pushd wgpu-raw-hs-codegen/wgpu-native
     WGPU_NATIVE_VERSION='v0.9.2.2' make lib-native
     popd
     ```
     
  3. Set `LD_LIBRARY_PATH` to include the Rust libraries that were just built.
     The Rust build steps will have produced a dynamic library, called
     `libwgpu_native.dylib` on macOS. The example code loads this library at
     runtime. `LD_LIBRARY_PATH` is a standard environment variable that allows
     the system dynamic loader to find the library.
  
     ```
     export LD_LIBRARY_PATH=$(pwd)/wgpu-raw-hs-codegen/wgpu-native/target/debug/:$LD_LIBRARY_PATH
     ```
     
  4. Build and run the `triangle` example:
  
     ```
     export METAL_DEVICE_WRAPPER_TYPE=1
     cabal run triangle
     ```
     
     The environment variable `METAL_DEVICE_WRAPPER_TYPE` enables Metal API
     validation.
     
If everything went well, you should see the initial triangle demo:

![triangle demo](triangle-demo.png)

